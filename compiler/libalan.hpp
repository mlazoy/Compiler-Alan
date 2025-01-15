#ifndef __LIBALAN_HPP__
#define __LIBALAN_HPP__

#include <cstdio>
#include <cstdint>

namespace libalan {
    extern "C" {
        void writeInteger(int n);
        void writeByte(uint8_t a);
        void writeChar(char c);
        void writeString(const uint8_t *s);
        int readInteger();
        uint8_t readByte();
        char readChar();
        void readString(int n, uint8_t* buf);
        uint8_t shrink(int i);
        int extend (uint8_t b);
        int strlen(const uint8_t *s);
        int strcmp(const uint8_t *s1, const uint8_t *s2);
        void strcpy(uint8_t *trg, const uint8_t *src);
        void strcat(uint8_t *trg, const uint8_t *src);
    }
}

#endif // __LIBALAN_HPP__