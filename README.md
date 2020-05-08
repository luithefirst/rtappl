# Real-time Approximation of Photometric Polygonal Lights

![Screenshot](screenshot.jpg)

## How to build

The demo requires the .NET Core 3.0 SDK. Nuget packages are managed using `paket` (https://fsprojects.github.io/Paket/). Paket needs to be installed as local dotnet tool using the .NET Core CLI and the nuget packages need to be restored before the build.

`dotnet tool install Paket --tool-path .paket`

`.paket\paket.exe restore`

`dotnet build Demo.sln`

You can also use the supplied build script by executing `build` in the command prompt. The script uses [aardvark.fake](https://github.com/aardvark-platform/aardvark.fake) with extended functionality.

The `Demo.exe` is built to `bin\Release\netcoreapp3.1\` and can be run from this directory.
