# Development and Contribution Info

## Workspace Setup

- This project uses aftman to manage tooling.
- This project uses rojo to build and test the library.
- This project uses lune to run development scripts.
- This project uses wally to manage dependencies, plus wally-package-types and wally-patch-package.

## Upgrading

Ideally all major versions of GreenTea should use the same internal objects so that
`GreenTea.isGreenTeaType` is true across all objects. This creates a splot between major versions:
- "major-major" versions: these have breaking changes that don't allow us to unify the objects types
- "major-minor" versions: these have breaking changes that allow us to unify the objects types

### Major-minor Versions

We can use a [semver-trick](https://github.com/dtolnay/semver-trick) to help unify internal objects.

For example, say we want to change `GreenTea.string({ graphemes = "..." })` from not requiring `bytes` to be defined to requiring it to be defined (as it is now):
- `v1.0.0` _does not_ require the user to specify `bytes`.
- `v2.0.0` _does_ require the user to specify `bytes`.
- `v1.0.1` takes `V2.0.0` as a dependency, and returns a copy of `v2.0.0` with `GreenTea.string` modified to
  include a default value for `bytes` if it is not specified.

In this manner, `v1.0.1`'s internal objects are exactly the same as `v2.0.0`'s, and they'll be
`GreenTea.isGreenTeaType` compatible.

## Major-major Versions

For these, it's mostly like a typical major upgrade, except we should make `GreenTea.typeof` take in previous versions' types and convert them. This gives an option for libraries expecting GreenTea types
to compose with libraries that use previous versions.

Ideally, we should also do the inverse: make the previous version able to take in a newer GreenTea's Type and compose with it.

## Wally

Wally is used to manage dependencies, but we also need types and patches.

The easiest way to do this is to run the lune script: `lune run wally-install` .
Alternatively, go read the lune script and see what it does.

We only use package patches to fix some output issues with jest-lua.

## Tests

Tests are ran using jest-lua. These require a fflag to be set.

A Lune script exists to set this flag: `lune run enable-loadmodule` . This...
- Finds the Roblox install directory, and the Roblox Studio's version folder
- Finds or creates `ClientSettings/ClientAppSettings.json`
- Sets the flag in that file
- Sets the file to read-only, so Roblox doesn't overwrite it on startup.

This is only implemented for Windows, because I only have a Windows machine.
If you have a Mac feel free to implement this for your platform in the lune script.

## Luau Rules

We disable LocalShadow because it's the only way to redefine a variable's types.
We'd like to remove this when it's not longer necessary.

## Doc Comments

Doc comments should be compatible with both moonwave and with luau-lsp.

This poses a problem for newlines: luau-lsp only works with `\` for newlines, but moonwave only works with double-spaces for newlines.

For now, we combine these two. This makes the moonwave docs look funny, because it won't omit the `\` before newlines, but 🤷‍♀️ what can you do.

# License Credits

## Favicon

The favicon is [from Microsoft's emoji set, under the MIT license.](https://github.com/microsoft/fluentui-emoji/blob/main/LICENSE)

<details><summary>See the license</summary>

MIT License

Copyright (c) Microsoft Corporation.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE

</details>