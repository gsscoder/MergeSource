# MergeSource
Merge all C# project source files into a single one.

# Usage
From terminal:
```bash
$ git clone https://github.com/gsscoder/commandline CommandLine
$ cd CommandLine/src/CommandLine
$ mono MergeSource.exe -p CommandLine.csproj -o $HOME/YourNewProject/CompleteSourceOfCommandLine.cs
```
Now you can include `CompleteSourceOfCommandLine.cs` into your C# project instead of adding reference with NuGet.
