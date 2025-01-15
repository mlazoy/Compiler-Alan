#include "libalan.hpp"

namespace libalan {

    extern "C" {

        void writeInteger(int n) {
            printf("%d",n);
        }

        void writeByte(uint8_t b) {
            printf("%hhu", b);
        }

        void writeChar(char c) {
            printf("%c", c);
        }

        void writeString(const uint8_t *s) {
            printf("%s", s);
        }

        int readInteger() {
            int n;
            scanf("%d", &n);
            return n;
        }

        uint8_t readByte() {
            uint8_t b;
            scanf("%hhu", &b);
            return b;
        }

        char readChar() {
            char c;
            scanf("%c", &c);
            return c;
        }

        void readString(int n, uint8_t* buf) {
            for (int i = 0; i < n-1; i++){
                int c = getchar();
                if (c == '\n' || c == EOF) break;
                *buf++ = uint8_t(c);
            }
            *buf = '\0'; 
        }

        uint8_t shrink(int i) {
            int mask = 0xff;
            return i & mask;
        }

        int extend (uint8_t b) {
            return int(b);
        }

        int strlen(const uint8_t *s){
            int sz = 0;
            while(*s++ != '\0') sz++;
            return sz;
        }

        int strcmp(const uint8_t *s1, const uint8_t *s2){
            while (*s1 == *s2 && *s1 != '\0'){
                s1++;
                s2++;
            }
            if(*s1 == '\0' && *s2 =='\0') return 0;
            else if(*s1 == '\0') return -1; //s1 is shorter
            else if (*s2 == '\0') return 1; //s2 is shorter 
            else return (*s1 < *s2) ? -1 : 1;
        }

        void strcpy(uint8_t *trg, const uint8_t *src){
            while(*src != '\0')
                *trg++ = *src++;
            *trg = '\0';
        }

        void strcat(uint8_t *trg, const uint8_t *src){
            while (*trg != '\0') trg++;
            while(*src != '\0')
                *(trg++) = *src++;
            *trg = '\0';
        }

    }

}