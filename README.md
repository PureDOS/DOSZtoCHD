# DOSZtoCHD
This tool converts a ZIP/DOSZ file to a CHD/BIN/CUE/ISO CD-ROM image file.

## Usage
The command line tool will print the program usage when being run without an argument. At least the input file needs to be specified. These are the options:

- `[-i] <PATH>`: Path to input DOSZ/ZIP file (required)
- `[-o] <PATH>`: Path to output file with CHD/CUE/ISO extension (optional, defaults to input file with a CHD extension)
- `-l <LABEL> `: Specify label
- `-f         `: Force overwrite existing files

## Download
You can find the download for Windows under the [Releases page](../../releases/latest).  
Just extract it and drag a .DOSZ file over DOSZtoCHD.exe to convert it.

For other platforms, see the section below on how to compile the command line tool.

## Compiling
On Windows you can use Visual Studio to compile both GUI and command line tool.

For other platforms, use either `./build-gcc.sh` or `./build-clang.sh` to compile the command line tool for your system.

## License
DOSZtoCHD is available under the [GNU General Public License, version 2 or later](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).
