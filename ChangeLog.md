# Changelog for dear-imgui

## [2.4.0]

- `imgui` updated to [1.92.9b].
    * Breaking: `ImTextureID` switched to a Word64 as a base type (was: pointer).
      + Remove your *ToPtr casts and just use ImTextureID type (a Word64 alias).
    * Breaking: `setGlyphExtraSpacing` renamed to `setGlyphExtraAdvanceX`.

## [2.3.1]

- Extended DragDrop API.
  * Added `DearImGui.withDragDropSource` and `DearImGui.withDragDropTarget` wrappers.
  * Fixed `DearImGui.Raw.DragDrop.beginTarget` to return accept flag.
  * Added `DearImGui.Raw.DragDrop.getData` and `DearImGui.Raw.DragDrop.getDataSize`.
  * Added remaining `Payload` internals.

## [2.3.0]

- `imgui` updated to [1.90.9].
    * Breaking: `sdlRendererRenderDrawData` now required `Renderer` arg.
    * Breaking: ImplVulkan removed command buffer for `ImGui_ImplVulkan_CreateFontsTexture`.
    * Breaking: ImplVulkan removed command for `ImGui_ImplVulkan_DestroyFontUploadObjects`.
      + Added `ImGui_ImplVulkan_DestroyFontsTexture`, but it shouldn't be needed as it is called from impl internals.
    * Breaking: ImplVulkan moved RenderPass into InitInfo structure.
      + Breaking: Haskell API is now using `Either RenderPass RenderingPipelineCreateInfo` to switch between RP/dynamic rendering.
- Added lots of missing widgets and their wrappers.
- Breaking: a few functions in `DearImGui` switched away from using CFloat/CBool wrappers.

## [2.2.1]

- Added `DearImGui.SDL.Renderer` backend and `sdlrenderer` example.
- Added `DearImgui.withCloseableWindow`.
- Added `DearImgui.Raw.framerate`.
- Added dynamic rendering and color attachment format options for `DearImGui.Vulkan` backend.
- Fixed Windows builds by using `system-cxx-std-lib` for GHC>=9.4.

## [2.2.0]

- `imgui` updated to [1.89.9].
- Update bounds for ghc-9.6.
- Exposed `DearImGui.Raw.Context`.
- Added `getCursorPos``.
- Fix TabItem flags type.

## [2.1.3]

- Fixed off-by-1 in internal Text wrapper.
- Fixed sliderFloat* Raw calls
- Added `formatPtr` to Raw.dragFloat* and Raw.sliderFloat*

## [2.1.2]

- Fixed glfw example build flags.
- Added `plotLines`.
- Added `setNextItemOpen`.

## [2.1.1]

- Build flag fix for MacOS.

## [2.1.0]

- `imgui` updated to [1.88].
    * Breaking: `ImGuiKeyModFlags` renamed to `ImGuiModFlags`.

## [2.0.0]

- `String` arguments replaced with `Text`.
  * Upgrading to `text-2` recommended to reap the UTF-8 benefits.

## [1.5.0]

- Added table wrappers.
- Added popup wrappers.
- Added `selectableWith`/`SelectableOptions` to expose optional arguments.
- Fix GHC-9.2 compatibility.

## [1.4.0]

- `imgui` updated to [1.87].
- Added `DearImGui.Vulkan.vulkanAddTexture`.
- Added `DearImGui.GLFW.glfwCursorPosCallback`.
  * Apps that don't install backend callbacks, *must* call it themselves.
- Added flags `use-wchar32` (default on) and `disable-obsolete` (default off).

## [1.3.1]

- `imgui` updated to [1.86].

## [1.3.0]

- Added `DearImGui.FontAtlas` and related `DearImGui.Raw.Font.*` bits.
- Removed old font atlas functions from `DearImGui` and `DearImGui.Raw`.

## [1.2.2]

- `imgui` updated to [1.85].

## [1.2.1]

- Added `DearImGui.Raw.DrawList` for drawing primitives.
- Added `DearImGui.Raw.IO` with attribute setters.
- Added `DearImGui.Raw.ListClipper` for efficient list viewports.

## [1.2.0]

- Fixed `nullPtr` in place of default arguments.
- Added functions for getting window position and size.
- Added `invisibleButton`.
- Added `inputTextMultiline` and `inputTextWithHint`.
- Changed `beginChild` and related `withChild*` to use full arguments.
- Added `withChildContext` to run actions inside other child window.
- Added `getCurrentContext`, `setCurrentContext`.
- Added `image` and `imageButton`.
- Added font atlas utilities.

## [1.1.0]

- `imgui` updated to [1.84.2].
- Removed unused Window argument from SDL `newFrame` to match 1.84.
- Added GLFW backend callbacks.
- Added more withXXX wrappers.

## [1.0.2]

- Added `withID` and `ToID(..)` to make composable components possible.

## [1.0.1]

- Fixed missing headers in source dist.

## [1.0.0]

Initial Hackage release based on [1.83].

[1.0.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.0.0
[1.0.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.0.1
[1.0.2]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.0.2
[1.1.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.1.0
[1.2.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.2.0
[1.2.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.2.1
[1.2.2]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.2.2
[1.3.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.3.0
[1.3.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.3.1
[1.4.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.4.0
[1.5.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.5.0
[2.0.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.0.0
[2.1.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.1.0
[2.1.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.1.1
[2.1.2]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.1.2
[2.1.3]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.1.3
[2.2.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.2.0
[2.2.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.2.1
[2.3.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.3.0
[2.3.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.3.1

[1.90.9]: https://github.com/ocornut/imgui/releases/tag/v1.90.9
[1.89.9]: https://github.com/ocornut/imgui/releases/tag/v1.89.9
[1.87]: https://github.com/ocornut/imgui/releases/tag/v1.87
[1.86]: https://github.com/ocornut/imgui/releases/tag/v1.86
[1.85]: https://github.com/ocornut/imgui/releases/tag/v1.85
[1.84.2]: https://github.com/ocornut/imgui/releases/tag/v1.84.2
[1.83]: https://github.com/ocornut/imgui/releases/tag/v1.83
