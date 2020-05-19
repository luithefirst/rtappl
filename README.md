# Real-time Approximation of Photometric Polygonal Lights

This repository contains a demo application as supplemental material to the publication ["Real-time Approximation of Photometric Polygonal Lights"](https://rtappl.vrvis.at/) that is part of the I3D 2020 paper program and published in the PACMCGIT journal.

The demo is implemented in F# and is baed on the [Aardvark plattform](https://github.com/aardvark-platform). It uses [FShade](https://www.fshade.org/), a DSL for shader programming. The main shading procedure of the cubature technique presentend in the paper can be found in [Cubature.fs](https://github.com/luithefirst/rtappl/blob/master/src/Cubature.fs#L95).

![Screenshot](screenshot.jpg)

## How to build

The demo requires the .NET Core 3.0 SDK. It manages Nuget packages using `paket` (https://fsprojects.github.io/Paket/) that needs to be installed as local dotnet tool using the .NET Core CLI. The packages need to be restored before the build.

`dotnet tool install Paket --tool-path .paket`

`.paket\paket.exe restore`

`dotnet build Demo.sln`

You can also use the supplied build script by executing `build` in the command prompt. The script uses [aardvark.fake](https://github.com/aardvark-platform/aardvark.fake) with extended functionality.

The `Demo.exe` is built to `bin\Release\netcoreapp3.1\` and can be run from this directory.
