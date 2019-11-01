# FScheme

An experimental scheme implementation based on [Write You a Scheme V2](https://github.com/write-you-a-scheme-v2/scheme), but writing in F#.

## Development

- Start

```powershell
git clone https://github.com/nodew/FScheme.git

cd FScheme
```

- Build

```powershell
dotnet build
```

- Run exe

```
dotnet run -p FScheme
```

- Test

```powershell
dotnet test
```

- Publish x64

```powershell
dotnet publish FScheme  -r win-x86 -c Release -o ..\bin  /p:PublishSingleFile=true /p:PublishTrimmed=true
```

- Publish x64

```powershell
dotnet publish FScheme  -r win-x64 -c Release -o ..\bin  /p:PublishSingleFile=true /p:PublishTrimmed=true
```

## TODO

- [] More primitives
- [] More built-in functions
- [] Pretty print
- [] Test coverage
- [] Macro