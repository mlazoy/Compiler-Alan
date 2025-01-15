#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdio>
#include <filesystem>
#include <string>
#include <cstring>


const char *cc_path = "../compiler/alan";
const char *lib_path = "../compiler/libalan.a";

const char *alan_suffix = ".alan";
const char *imm_suffix = ".imm";
const char *asm_suffix = ".asm";
const char *exec_suffix = ".out";

void printUsage(const char *program) {
    std::cerr << "Usage: " << program << " [-O] [-f] [-i] [-d] <source_file>\n"
              << "  -O          Enables optimization\n"
              << "  -f          Reads program from stdin; Outputs final code to stdout\n"
              << "  -i          Reads program from stding; Outputs intermediate code to stdout\n"
              << "  -d          Debug mode: Creates *.ast and *.symbol files in current directory\n"
              << "  --version   \n"
              << "  --opaque    Triggers llc --opaque-pointers flag if version <15\n"
              << "  --pie       Disables clang -no-pie flag \n\n"
              << "NOTE: When options [-f] [-i] are selected executable is saved into /tmp/temp.alan\n";
              ;
    exit(1);
}

void printVersion() {
    std::cerr << "alanc : The Alan Compiler v1.0.0\n";
    exit(0);
}

bool is_alan(const char *filename) {
    int f_len = strlen(filename);
    int as_len = strlen(alan_suffix);
    if (f_len <= as_len) return 0;
    int offset = f_len - as_len;
    return strcmp(filename+offset, alan_suffix) == 0;
}

void print_assembly(const std::string &filename) {
    std::ifstream file(filename);
    if (!file) {
        std::cerr << "Error: Could not open file " << filename << "\n";
        return;
    }

    std::string line;
    while (std::getline(file, line)) {
        std::cout << line << "\n";  // Print each line
    }
    file.close();
}

int main(int argc, char *argv[]) {
    bool o_flag, f_flag, i_flag, d_flag, opaq_flag, pie_flag;
    const char *sourcefile = nullptr;
    char *flag;

    o_flag = f_flag = i_flag = d_flag = opaq_flag = pie_flag = false;

    if (argc <= 1 || argc > 8) {
        std:: cerr <<"Wrong number of arguements\n\n";
        printUsage(argv[0]);
    }

    if (strcmp(argv[1],"--help") == 0) printUsage(argv[0]);
    else if (strcmp(argv[1],"--version") == 0) printVersion();

    for (int i=1; i < argc; ++i){ 
        flag = argv[i];
        if (flag[0] == '-'){
            if (strlen(flag) == 2) {
                switch (flag[1]){
                    case 'O' : o_flag = true; break;
                    case 'f' : f_flag = true; break;
                    case 'i' : i_flag = true; break;
                    case 'd' : d_flag = true; break;
                    default  : std::cerr << "Unknown flag " << flag << "\n\n"; 
                            printUsage(argv[0]);
                }
            }
            else if (strcmp(flag, "--pie") == 0) pie_flag = true;
            else if (strcmp(flag, "--opaque") == 0) opaq_flag = true;
            else {
                std::cerr << "Unknown flag " << flag << "\n\n"; 
                printUsage(argv[0]);
            }
        }
        //else printUsage(argv[0]);
    }

    if ( !f_flag && !i_flag ) {
        sourcefile = argv[argc-1]; // Last parameter is the source file
        if (!is_alan(sourcefile)) {
            std::cerr << "Invalid input file; provide an '.alan' one\n\n";
            printUsage(argv[0]);
        }
    } 
    else {
        sourcefile = "/tmp/temp.alan"; // Temporary file to store stdin content
        std::ofstream temp_file(sourcefile);
        if (!temp_file) {
            std::cerr << "Error: Could not create temporary file\n";
            return 1;
        }
        std::string line;
        while (std::getline(std::cin, line)) {
            temp_file << line << "\n";
        }
        temp_file.close();
    }

    // Derive intermediate, assembly and executable file names
    std::string base_name, file_name, imm_file, asm_file, exec_file;

    std::filesystem::path source_path(sourcefile);
    file_name = source_path.filename().string();    //removes full path
    size_t last_dot = std::string(sourcefile).rfind('.');
    base_name = std::string(sourcefile).substr(0, last_dot);
    imm_file = base_name + imm_suffix;
    asm_file = base_name + asm_suffix;
    exec_file = base_name + exec_suffix; //TODO! a.out instead ?

    // std::cout << sourcefile << " " << base_name << " " 
    //           << imm_file << " " << asm_file << " " << exec_file << "\n\n";

    std::string cmd;
    // ./"$ALAN_PATH"/alan < $1 > a.imm
    cmd = std::string(cc_path) + " " + file_name; //pass sourcefile as 1st arguement
    if (o_flag) cmd += " -O";
    if (d_flag) cmd += " -d";
    cmd += " < " + std::string(sourcefile) + " > " + imm_file;
    int status = std::system(cmd.c_str());
    if (status!= 0 && status != 42) { // 42 is exit status of caught error from Alan
        std::cerr << "Failed to generate intermediate code\n";
        return 1;
    }
    // llc -o a.asm a.imm
    if (opaq_flag) cmd = "llc --opaque-pointers -o ";
    else cmd = "llc -o ";
    cmd+= asm_file + " " + imm_file;
    if (std::system(cmd.c_str()) != 0) {
        std::cerr << "Failed to generate final code\n";
        return 1;
    }
    // clang++ -o a.out a.asm "$ALAN_PATH"/libalan.a -no-pie -stdlib=libc++
    cmd = "clang++ -o " + exec_file + " " + asm_file + " " + lib_path;
    if(pie_flag) cmd += " -stdlib=libc++";
    else cmd += " -no-pie -stdlib=libc++";
    if (std::system(cmd.c_str()) != 0) {
        std::cerr << "Failed to generate the executable\n";
        return 1;
    }

    if(i_flag) print_assembly(imm_file);
    if (f_flag) print_assembly(asm_file);

    return 0;

}