#include "pch.h"
#include <future>
#include <winrt/Windows.Foundation.h>
#include <winrt/Windows.Storage.h>
#include <winrt/Windows.Security.Cryptography.h>
#include <XrUtility/XrString.h>
#include <XrUtility/XrSceneUnderstanding.h>
#include <pbr/GltfLoader.h>
#include <SampleShared/FileUtility.h>
#include <XrSceneLib/PbrModelObject.h>
#include <XrSceneLib/Scene.h>
#include <XrSceneLib/SpaceObject.h>

using namespace std::chrono_literals;
using TimePoint = engine::FrameTime::clock::time_point;

namespace wrt {
    using namespace winrt::Windows::Storage;
    using namespace winrt::Windows::Storage::Streams;
    using winrt::Windows::Foundation::AsyncStatus;
    using winrt::Windows::Foundation::IAsyncOperation;
    using winrt::Windows::Security::Cryptography::CryptographicBuffer;
} // namespace wrt

namespace {
    const winrt::hstring SceneFileName(L"su_scene.bin");
    constexpr auto UpdateInterval = 2s; // Time to wait between SU requests
    constexpr float ScanRadius = 4.8f;  // meters

    struct SceneVisuals {
        SceneVisuals() = default;
        SceneVisuals(std::vector<std::shared_ptr<engine::Object>> visuals, std::vector<XrSceneObjectKeyMSFT> keys, xr::SceneHandle scene)
            : visuals(std::move(visuals))
            , keys(std::move(keys))
            , scene(std::move(scene)) {
        }

        std::vector<std::shared_ptr<engine::Object>> visuals;
        std::vector<XrSceneObjectKeyMSFT> keys;
        xr::SceneHandle scene;
    };

    void ComputeNewScene(const xr::ExtensionContext& extensions,
                         XrSceneObserverMSFT sceneObserver,
                         XrSpace space,
                         XrTime time,
                         const XrSceneSphereBoundMSFT& sphere);
    SceneVisuals CreateSceneVisuals(const xr::ExtensionContext& extensions,
                                    const Pbr::Resources& pbrResources,
                                    const std::shared_ptr<Pbr::Material>& material,
                                    xr::SceneHandle scene);
    wrt::IAsyncOperation<wrt::IBuffer> ReadBufferAsync(winrt::hstring filename);
    void SerializeScene(const xr::ExtensionContext& extensions,
                        xr::SceneObserverHandle&& sceneObserver,
                        XrSpace space,
                        XrTime time,
                        const XrSceneSphereBoundMSFT& sphere);

    struct PlaneFindingScene : public engine::Scene {
        explicit PlaneFindingScene(engine::Context& context)
            : Scene(context)
            , m_extensions(context.Extensions)
            , m_meshMaterial{Pbr::Material::CreateFlat(m_context.PbrResources, Pbr::FromSRGB(DirectX::Colors::White), 1.0f, 0.0f)}
            , m_nextUpdate{engine::FrameTime::clock::now() + UpdateInterval} {
            XrReferenceSpaceCreateInfo spaceCreateInfo{XR_TYPE_REFERENCE_SPACE_CREATE_INFO};
            spaceCreateInfo.referenceSpaceType = XR_REFERENCE_SPACE_TYPE_VIEW;
            spaceCreateInfo.poseInReferenceSpace = xr::math::Pose::Identity();
            CHECK_XRCMD(xrCreateReferenceSpace(context.Session.Handle, &spaceCreateInfo, m_viewSpace.Put()));
        }

        ~PlaneFindingScene() override {
            // Stop the worker thread first before destroying this class
            if (m_future.valid()) {
                m_future.get();
            }
            if (m_serializeFuture.valid()) {
                m_serializeFuture.get();
            }
        }

        void OnUpdate(const engine::FrameTime& frameTime) override {
            m_lastTimeOfUpdate = frameTime.PredictedDisplayTime;
            // Check if the background thread finished creating a new group of scene objects.
            if (m_scanState == ScanState::Processing && m_future.valid() && m_future.wait_for(0s) == std::future_status::ready) {
                for (const std::shared_ptr<engine::Object>& object : m_sceneVisuals.visuals) {
                    RemoveObject(object);
                }
                m_sceneVisuals = m_future.get();
                for (const std::shared_ptr<engine::Object>& object : m_sceneVisuals.visuals) {
                    AddObject(object);
                    object->SetVisible(false);
                }
                m_scanState = ScanState::Idle;
                m_nextUpdate = frameTime.Now + UpdateInterval;
            }

            if (m_sceneVisuals.scene) {
                XrSceneObjectsLocateInfoMSFT locateInfo{XR_TYPE_SCENE_OBJECTS_LOCATE_INFO_MSFT};
                locateInfo.baseSpace = m_context.SceneSpace;
                locateInfo.time = frameTime.PredictedDisplayTime;
                locateInfo.sceneObjectCount = static_cast<uint32_t>(m_sceneVisuals.keys.size());
                locateInfo.sceneObjectKeys = m_sceneVisuals.keys.data();

                m_sceneObjectLocations.resize(m_sceneVisuals.keys.size());
                XrSceneObjectLocationsMSFT locations{XR_TYPE_SCENE_OBJECT_LOCATIONS_MSFT};
                locations.sceneObjectCount = static_cast<uint32_t>(m_sceneObjectLocations.size());
                locations.sceneObjectLocations = m_sceneObjectLocations.data();

                CHECK_XRCMD(m_extensions.xrLocateSceneObjectsMSFT(m_sceneVisuals.scene.Get(), &locateInfo, &locations));
                for (size_t i = 0; i < m_sceneVisuals.keys.size(); ++i) {
                    const XrSceneObjectLocationMSFT& location = m_sceneObjectLocations[i];
                    const auto& object = m_sceneVisuals.visuals[i];
                    if (xr::math::Pose::IsPoseValid(location.locationFlags)) {
                        object->Pose() = location.pose;
                        object->SetVisible(true);
                    } else {
                        object->SetVisible(false);
                    }
                }
            }

            XrSpaceLocation viewInLocal{XR_TYPE_SPACE_LOCATION};
            CHECK_XRCMD(xrLocateSpace(m_viewSpace.Get(), m_context.SceneSpace, frameTime.PredictedDisplayTime, &viewInLocal));
            if (xr::math::Pose::IsPoseValid(viewInLocal)) {
                m_sphere.center = viewInLocal.pose.position;
                m_sphere.radius = ScanRadius;
            }

            if (m_readSceneTask != nullptr) {
                if (m_readSceneTask.Status() == wrt::AsyncStatus::Completed) {
                    // Deserialize
                    wrt::IBuffer buffer = m_readSceneTask.get();
                    winrt::com_array<uint8_t> data;
                    wrt::CryptographicBuffer::CopyToByteArray(buffer, data);

                    XrDeserializeSceneFragmentMSFT fragment{static_cast<uint32_t>(data.size()), data.data()};
                    XrDeserializeSceneInfoMSFT deserializeInfo{XR_TYPE_DESERIALIZE_SCENE_INFO_MSFT};
                    deserializeInfo.fragmentCount = 1;
                    deserializeInfo.fragments = &fragment;
                    CHECK_XRCMD(m_extensions.xrDeserializeSceneMSFT(m_sceneObserver.Get(), &deserializeInfo));

                    m_scanState = ScanState::Waiting;
                }
                if (m_readSceneTask.Status() != wrt::AsyncStatus::Started) {
                    m_readSceneTask = nullptr;
                }
            } else if (m_scanState == ScanState::Waiting) {
                // Check if the results are available
                XrSceneComputeStateMSFT state{};
                CHECK_XRCMD(m_extensions.xrGetSceneComputeStateMSFT(m_sceneObserver.Get(), &state));
                if (state == XR_SCENE_COMPUTE_STATE_COMPLETED_MSFT) {
                    xr::SceneHandle scene = xr::CreateScene(m_extensions, m_sceneObserver.Get());
                    // Send the scene compute result to the background thread for processing
                    m_future = std::async(std::launch::async,
                                          &CreateSceneVisuals,
                                          std::cref(m_extensions),
                                          std::cref(m_context.PbrResources),
                                          m_meshMaterial,
                                          std::move(scene));
                    m_scanState = ScanState::Processing;
                }
            } else if (m_scanState == ScanState::Idle) {
                // no active query, start one if enough time has passed
                if (frameTime.Now > m_nextUpdate) {
                    ComputeNewScene(m_extensions, m_sceneObserver.Get(), m_context.SceneSpace, m_lastTimeOfUpdate, m_sphere);
                    m_nextUpdate = frameTime.Now + UpdateInterval;
                    m_scanState = ScanState::Waiting;
                }
            }
        }

        void OnActiveChanged() override {
            if (IsActive()) {
                Enable();
            } else {
                Disable();
            }
        }

    private:
        enum class ScanState { Idle, Waiting, Processing };

        void Enable() {
            m_sceneObserver = xr::CreateSceneObserver(m_extensions, m_context.Session.Handle);
            m_scanState = ScanState::Idle;
            m_readSceneTask = ReadBufferAsync(SceneFileName);
        }

        void Disable() {
            for (const std::shared_ptr<engine::Object>& object : m_sceneVisuals.visuals) {
                RemoveObject(object);
            }
            m_sceneVisuals = {};

            // Stop the worker thread before clearing sceneObserver because the thread has access to it.
            if (m_future.valid()) {
                m_future.get();
            }
            if (m_sceneObserver) {
                m_serializeFuture = std::async(std::launch::async,
                                               &SerializeScene,
                                               std::cref(m_extensions),
                                               std::move(m_sceneObserver),
                                               m_context.SceneSpace,
                                               m_lastTimeOfUpdate,
                                               std::cref(m_sphere));
            }
        }

        const xr::ExtensionContext& m_extensions;
        const std::shared_ptr<Pbr::Material> m_meshMaterial;
        SceneVisuals m_sceneVisuals;
        xr::SceneObserverHandle m_sceneObserver;
        xr::SpaceHandle m_viewSpace;
        XrTime m_lastTimeOfUpdate{};
        XrSceneSphereBoundMSFT m_sphere{};
        std::vector<XrSceneObjectLocationMSFT> m_sceneObjectLocations;
        std::future<SceneVisuals> m_future;
        std::future<void> m_serializeFuture;
        TimePoint m_nextUpdate{};
        ScanState m_scanState{ScanState::Idle};
        wrt::IAsyncOperation<wrt::IBuffer> m_readSceneTask{nullptr};
    };

    Pbr::RGBAColor GetColor(XrSceneObjectKindTypeMSFT kind) {
        using namespace DirectX;
        // The lighting system makes a lot of the colors too bright so multiply them to tone them down
        constexpr auto scaleColor = [](const XMVECTORF32& color, float scale) {
            return Pbr::FromSRGB(color * XMVECTORF32{scale, scale, scale, 1});
        };
        switch (kind) {
        case XR_SCENE_OBJECT_KIND_TYPE_CEILING_MSFT:
            return Pbr::FromSRGB(Colors::Green);
        case XR_SCENE_OBJECT_KIND_TYPE_FLOOR_MSFT:
            return scaleColor(Colors::Blue, 0.5f);
        case XR_SCENE_OBJECT_KIND_TYPE_PLATFORM_MSFT:
            return scaleColor(Colors::Orange, 0.6f);
        case XR_SCENE_OBJECT_KIND_TYPE_WALL_MSFT:
            return scaleColor(Colors::Tomato, 0.5f);
        case XR_SCENE_OBJECT_KIND_TYPE_BACKGROUND_MSFT:
            return scaleColor(Colors::Cyan, 0.8f);
        case XR_SCENE_OBJECT_KIND_TYPE_UNKNOWN_MSFT:
            return scaleColor(Colors::Purple, 0.8f);
        default:
            return Pbr::FromSRGB(Colors::White);
        }
    }

    void FillMeshPrimitiveBuilder(const std::vector<XrVector3f>& positions,
                                  const std::vector<uint32_t>& indices,
                                  const Pbr::RGBAColor& color,
                                  Pbr::PrimitiveBuilder& builder) {
        using namespace DirectX;
        const size_t indexCount = indices.size();
        builder.Vertices.clear();
        builder.Indices.clear();
        builder.Vertices.reserve(indexCount);
        builder.Indices.reserve(indexCount);

        auto appendVertex = [&builder](const XMVECTOR& pos, Pbr::Vertex& vertex) {
            XMStoreFloat3(&vertex.Position, pos);
            builder.Indices.push_back(static_cast<uint32_t>(builder.Vertices.size()));
            builder.Vertices.push_back(vertex);
        };

        // Create 3 vertices per triangle where the normal is perpendicular to the surface
        // in order to make the triangle edges sharper.
        for (size_t index = 2; index < indices.size(); index += 3) {
            auto v0 = xr::math::LoadXrVector3(positions[indices[index - 2]]);
            auto v1 = xr::math::LoadXrVector3(positions[indices[index - 1]]);
            auto v2 = xr::math::LoadXrVector3(positions[indices[index]]);

            Pbr::Vertex vertex{};
            vertex.Color0 = color;
            XMStoreFloat4(&vertex.Tangent, XMVectorSetW(XMVector3Normalize(v1 - v0), 1.0f));
            // CCW winding order
            XMStoreFloat3(&vertex.Normal, XMVector3Normalize(XMVector3Cross(v1 - v0, v2 - v0)));
            appendVertex(v0, vertex);
            appendVertex(v2, vertex);
            appendVertex(v1, vertex);
        }
    }

    void ComputeNewScene(const xr::ExtensionContext& extensions,
                         XrSceneObserverMSFT sceneObserver,
                         XrSpace space,
                         XrTime time,
                         const XrSceneSphereBoundMSFT& sphere) {
        XrNewSceneComputeInfoMSFT computeInfo{XR_TYPE_NEW_SCENE_COMPUTE_INFO_MSFT};

        computeInfo.bounds.space = space;
        computeInfo.bounds.time = time;
        computeInfo.bounds.sphereCount = 1;
        computeInfo.bounds.spheres = &sphere;

        // Start the async query
        CHECK_XRCMD(extensions.xrComputeNewSceneMSFT(sceneObserver, &computeInfo));
    }

    wrt::IAsyncOperation<wrt::IBuffer> ReadBufferAsync(winrt::hstring filename) {
        wrt::StorageFolder storageFolder = wrt::ApplicationData::Current().LocalFolder();
        // GetFileAsync will throw an exception if the file doesn't exist, which will be captured by the IAsyncOperation.
        wrt::StorageFile file = co_await storageFolder.GetFileAsync(filename);
        co_return co_await wrt::FileIO::ReadBufferAsync(file);
    }

    std::shared_ptr<engine::Object> CreateMesh(const Pbr::Resources& pbrResources,
                                               const std::shared_ptr<Pbr::Material>& material,
                                               const Pbr::PrimitiveBuilder& builder) {
        auto model = std::make_shared<Pbr::Model>();
        model->AddPrimitive(Pbr::Primitive(pbrResources, builder, material));
        return std::make_shared<engine::PbrModelObject>(std::move(model));
    }

    std::shared_ptr<engine::Object> CreateSceneObjectVisual(const xr::ExtensionContext& extensions,
                                                            const Pbr::Resources& pbrResources,
                                                            const std::shared_ptr<Pbr::Material>& material,
                                                            XrSceneMSFT scene,
                                                            XrSceneObjectKeyMSFT sceneObjectKey,
                                                            XrSceneMeshKeyMSFT meshKey,
                                                            Pbr::PrimitiveBuilder& builder) {
        xr::SceneMesh mesh = GetSceneMesh(extensions, scene, meshKey);
        if (mesh.indices.empty() || mesh.positions.empty()) {
            return nullptr;
        }

        XrSceneObjectPropertiesMSFT properties{XR_TYPE_SCENE_OBJECT_PROPERTIES_MSFT};
        XrSceneObjectKindMSFT kind{XR_TYPE_SCENE_OBJECT_KIND_MSFT};
        xr::InsertExtensionStruct(properties, kind);

        XrSceneObjectPropertiesGetInfoMSFT getInfo{XR_TYPE_SCENE_OBJECT_PROPERTIES_GET_INFO_MSFT};
        getInfo.sceneObjectKey = sceneObjectKey;
        CHECK_XRCMD(extensions.xrGetSceneObjectPropertiesMSFT(scene, &getInfo, &properties));

        FillMeshPrimitiveBuilder(mesh.positions, mesh.indices, GetColor(kind.kind), builder);
        std::shared_ptr<engine::Object> object = CreateMesh(pbrResources, material, builder);
        return object;
    }

    SceneVisuals CreateSceneVisuals(const xr::ExtensionContext& extensions,
                                    const Pbr::Resources& pbrResources,
                                    const std::shared_ptr<Pbr::Material>& material,
                                    xr::SceneHandle scene) {
        std::vector<std::shared_ptr<engine::Object>> visuals;
        std::vector<XrSceneObjectKeyMSFT> keys;
        Pbr::PrimitiveBuilder builder;
        for (const XrSceneObjectMSFT& xrSceneObject : xr::GetSceneObjects(extensions, scene.Get())) {
            for (const XrSceneMeshKeyMSFT meshKey : xr::GetMeshKeys(extensions, scene.Get(), xrSceneObject.sceneObjectKey)) {
                std::shared_ptr<engine::Object> object = CreateSceneObjectVisual(
                    extensions, pbrResources, material, scene.Get(), xrSceneObject.sceneObjectKey, meshKey, builder);
                if (object != nullptr) {
                    visuals.push_back(std::move(object));
                    keys.push_back(xrSceneObject.sceneObjectKey);
                }
            }
        }
        return SceneVisuals(std::move(visuals), std::move(keys), std::move(scene));
    }

    void SerializeScene(const xr::ExtensionContext& extensions,
                        xr::SceneObserverHandle&& sceneObserver,
                        XrSpace space,
                        XrTime time,
                        const XrSceneSphereBoundMSFT& sphere) {
        assert(sceneObserver);
        XrSceneComputeStateMSFT state;
        do {
            // wait for the current query to finish
            std::this_thread::sleep_for(50ms);
            CHECK_XRCMD(extensions.xrGetSceneComputeStateMSFT(sceneObserver.Get(), &state));
        } while (state == XR_SCENE_COMPUTE_STATE_UPDATING_MSFT);

        XrNewSceneComputeInfoMSFT computeInfo{XR_TYPE_NEW_SCENE_COMPUTE_INFO_MSFT};

        // Chaining in XrSerializeSceneMSFT causes xrComputeNewSceneMSFT to serialize the scene.
        XrSerializeSceneMSFT serializeScene{XR_TYPE_SERIALIZE_SCENE_MSFT};
        xr::InsertExtensionStruct(computeInfo, serializeScene);

        computeInfo.bounds.space = space;
        computeInfo.bounds.time = time;
        computeInfo.bounds.sphereCount = 1;
        computeInfo.bounds.spheres = &sphere;

        // Start the async query
        CHECK_XRCMD(extensions.xrComputeNewSceneMSFT(sceneObserver.Get(), &computeInfo));

        for (;;) {
            std::this_thread::sleep_for(250ms);
            CHECK_XRCMD(extensions.xrGetSceneComputeStateMSFT(sceneObserver.Get(), &state));
            if (state == XR_SCENE_COMPUTE_STATE_COMPLETED_MSFT) {
                wrt::StorageFolder storageFolder{wrt::ApplicationData::Current().LocalFolder()};
                wrt::StorageFile storageFile =
                    storageFolder.CreateFileAsync(SceneFileName, wrt::CreationCollisionOption::ReplaceExisting).get();
                std::basic_ofstream<uint8_t> outfile;
                outfile.exceptions(std::ios::failbit | std::ios::badbit);
                outfile.open(storageFile.Path().c_str(), std::ofstream::binary);
                xr::SceneHandle scene = xr::CreateScene(extensions, sceneObserver.Get());
                xr::ReadSerializedScene(extensions, scene.Get(), outfile);
                outfile.close();
                break;
            }
        }
    }

} // namespace

std::unique_ptr<engine::Scene> TryCreatePlaneFindingScene(engine::Context& context) {
    return context.Extensions.SupportsSceneUnderstanding ? std::make_unique<PlaneFindingScene>(context) : nullptr;
}
