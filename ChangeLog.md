# Changelog for dear-imgui

## [1.2.2]

- `imgui` updated to 1.85.

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

- `imgui` updated to 1.84.2.
- Removed unused Window argument from SDL `newFrame` to match 1.84.
- Added GLFW backend callbacks.
- Added more withXXX wrappers.

## [1.0.2]

- Added `withID` and `ToID(..)` to make composable components possible.

## [1.0.1]

- Fixed missing headers in source dist.

## [1.0.0]

Initial Hackage release based on 1.83.

[1.0.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.0.0
[1.0.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.0.1
[1.0.2]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.0.2
[1.1.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.1.0
[1.2.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.2.0
[1.2.1]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.2.1
[1.2.2]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.2.2
[1.3.0]: https://github.com/haskell-game/dear-imgui.hs/tree/v1.3.0
