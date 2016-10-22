#!/usr/bin/env python3
"""
Changes the color of the buttons to
match the new graphic
"""
import sys
import os

def read_bitmap_and_color():
    f = sys.stdin.buffer
    bitmap = bytearray(f.read(392))
    color  = bytearray(f.read(49))
    return (bitmap, color)

def extract_color(color):
    i = 0
    for c in color:
        if i %7 == 0:
            sys.stderr.write(' - ')
        else:
            sys.stderr.write(',')
        i = i+1
        sys.stderr.write(str(c))
    color = color.replace(b'\x09',b'\x08')
    color = color.replace(b'\x03',b'\x06')
    color = color.replace(b'\x07',b'\x03')
    sys.stderr.write('\n')

    i = 0
    for c in color:
        if i %7 == 0:
            sys.stderr.write(' - ')
        else:
            sys.stderr.write(',')
        i = i+1
        sys.stderr.write(str(c))
    return color

def main():
    if len(sys.argv) != 1:
        print("Usage: %s < button.raw > new_button.raw" % __file__)
        sys.exit(1)


    bitmap, color = read_bitmap_and_color()
    new_colors = extract_color(color)

    sys.stdout.buffer.write(bitmap)
    sys.stdout.buffer.write(new_colors)

if __name__ == "__main__": main()
