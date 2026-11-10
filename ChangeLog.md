# Changelog for dear-imgui

## [3.0.0] -- WIP

The FFI layer is now provided by the generated [dear-imgui-raw] packages instead of hand-written inline-c wrappers.
`dear-imgui` keeps the high-level `DearImGui` and `DearImGui.FontAtlas` modules on top of them.

### Build and packaging

- Requires GHC 9.2+.
- `imgui` updated to [1.92.9b] (docking branch; docking and viewports are runtime opt-in).
- The `imgui` git submodule is gone. Sources are bundled with `dear-imgui-raw`, so `--recurse-submodules` is no longer needed.
  `dear-imgui-raw` and the backend packages are fetched from [dear-imgui-raw]; see `cabal.project` for the `source-repository-package` stanza.
- Breaking: cabal flags `opengl2`, `opengl3`, `vulkan`, `sdl`, `sdl-renderer` and `glfw` are gone.
  Platform and renderer backends are separate `dear-imgui-impl-<backend>` packages
  (`opengl2`, `opengl3`, `sdl2`, `sdlrenderer2`, `glfw`, `vulkan`); depend on the ones you use directly.
  They link the native SDL2/GLFW/Vulkan libraries; `dear-imgui` itself no longer depends on
  `sdl2`, `GLFW-b`, `bindings-GLFW` or `vulkan` and has no `pkgconfig-depends` or `extra-libraries`.
- `glew` is no longer required; OpenGL3 uses the imgui built-in loader. `ImGui_ImplOpenGL3_Init` no longer calls `glewInit()`,
  so if your code relied on that, initialize your GL loader yourself.
- Breaking: cabal flags `use-wchar32`, `use-ImDrawIdx32` and `disable-obsolete` are gone; all three are always on.

### Raw access

- Breaking: `DearImGui.Raw*` modules are gone. For raw access depend on `dear-imgui-raw` and use its per-struct modules
  (`DearImGui.Raw.ImGui`, `DearImGui.Raw.ImDrawList`, `DearImGui.Raw.ImGuiIO` field accessors like `poke io.iniFilename`, ...).
  Raw functions take the full imgui argument list and pass small structs by value, so `Foreign.with` wrapping is not needed.
- Breaking: prefixed enum patterns (`ImGuiWindowFlags_NoTitleBar`) are gone. Import the raw enum module qualified:
  `import qualified DearImGui.Raw.Enums.ImGuiWindowFlags as ImGuiWindowFlags` and write `ImGuiWindowFlags.NoTitleBar`
  (`ImGuiWindowFlags.None` or plain `0` for no flags).
  Enum types are plain integer aliases now (`CInt`; `ImU8` for `ImGuiSortDirection`), `FiniteEnum` is gone.
  `DearImGui.Enums` still re-exports the type names used in signatures.

### Types

- Breaking: `ImVec2`/`ImVec4`/`ImTextureRef`/`ImGuiTableSortSpecs`/`ImGuiTableColumnSortSpecs` are the raw records.
  Field selector functions (`x`, `y`, `z`, `w`, ...) are gone; use pattern matching or `OverloadedRecordDot`.
  `ImVec3` stays in `DearImGui.Structs`.
- Breaking: `ImTextureRef` fields are `_TexData :: Ptr ImTextureData` and `_TexID` (were `texData`/`texID`).
  `textureRefFromID` is still the recommended constructor.
- Breaking: handle newtypes became pointer synonyms: `Context = Ptr ImGuiContext`, `DrawData = Ptr ImDrawData`,
  `Font = Ptr ImFont`, `DrawList = Ptr ImDrawList`, `FontConfig = Ptr ImFontConfig`, `GlyphRanges = Ptr ImWchar`.
- Breaking: `ImGuiKeyChord` is `CInt` (was `Int`).

### High-level API

- Added `imageButton` and `beginItemTooltip`.
- Breaking: `image`/`imageWithBg` take `ImTextureRef`/`ImVec2`/`ImVec4` values instead of pointers.
- Breaking: `withTooltip` now wraps `beginTooltip` (was mistakenly using `beginItemTooltip`).
- Breaking: removed `logText`, `getWindowContentRegionMin`, `getWindowContentRegionMax`, `setColorEditOptions`,
  `FontAtlas.build` (obsolete in imgui 1.92 or not expressible without varargs). `FontAtlas.rebuild` no longer needs an explicit build step.
- `colorButton` no longer requires `HasSetter` on its colour reference.
- Breaking: `showStyleSelector`, `showFontSelector` and `tableHeader` take `Text` (were `CString`);
  `popStyleColor` and `setKeyboardFocusHere` take `Int` (were `CInt`);
  `plotLines`/`plotHistogram` take `[Float]`; font sizes in `withFontWithSize`/`withFontSize`/`pushFontWithSize`/`pushFontSize`
  and the drag threshold in `isMouseDragging`/`getMouseDragDelta` are `Float` (were `CFloat`).

### Backends

- Breaking: `DearImGui.OpenGL2`, `DearImGui.OpenGL3`, `DearImGui.SDL`, `DearImGui.SDL.*`, `DearImGui.GLFW`,
  `DearImGui.GLFW.*` and `DearImGui.Vulkan` are removed. Use the `DearImGui.Impl.*` bridge modules of the
  `dear-imgui-impl-<backend>` packages (or the `DearImGui.Raw.Impl.*` modules underneath them for raw pointers)
  and see their documentation for the new APIs.

[dear-imgui-raw]: https://gitlab.com/dpwiz/hsimgui

## [2.5.0]

- `imgui` updated to [1.92.8].
  * `image`: drops `border` and `tint` colors. Use `imageWithBg` instead.
  * textures funcs: Change params `ImTextureID` to `ImTextureRef`
  * rename: `pushTextureID` -> `pushTexture`, `popTextureID` -> `popTexture`
  * new: `addFontDefaultBitmap|Vector`
  * swap arguments in `addRect`, `addPolyLine`, `pathStroke`
- Added `inputPassword` wrapper for text input, which sets the flag.

## [2.4.1]

- Fixed builds on GHC-9.10+.

## [2.4.0]

- `imgui` updated to [1.91.9b].
    * Breaking: `ImTextureID` switched to a Word64 as a base type (was: pointer).
      + Remove your *ToPtr casts and just use ImTextureID type (a Word64 alias).
    * Breaking: `setGlyphExtraSpacing` renamed to `setGlyphExtraAdvanceX`.
    * Breaking: Some flags got updated, consult the [changelog](https://github.com/ocornut/imgui/blob/v1.91.9b/docs/CHANGELOG.txt) for migration hints.

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
[2.4.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.4.0
[2.4.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.4.1
[2.5.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v2.5.0
[3.0.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v3.0.0

[1.92.9b]: https://github.com/ocornut/imgui/releases/tag/v1.92.9b
[1.92.8]: https://github.com/ocornut/imgui/releases/tag/v1.92.8
[1.91.9b]: https://github.com/ocornut/imgui/releases/tag/v1.91.9b
[1.90.9]: https://github.com/ocornut/imgui/releases/tag/v1.90.9
[1.89.9]: https://github.com/ocornut/imgui/releases/tag/v1.89.9
[1.87]: https://github.com/ocornut/imgui/releases/tag/v1.87
[1.86]: https://github.com/ocornut/imgui/releases/tag/v1.86
[1.85]: https://github.com/ocornut/imgui/releases/tag/v1.85
[1.84.2]: https://github.com/ocornut/imgui/releases/tag/v1.84.2
[1.83]: https://github.com/ocornut/imgui/releases/tag/v1.83
