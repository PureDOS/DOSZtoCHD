echo Building \'DOSZtoCHD-`uname -m`\' ...
gcc -std=c++11 DOSZtoCHD.cpp -lstdc++ -Wall -Wno-psabi -O3 -o DOSZtoCHD-`uname -m`
echo Done!
