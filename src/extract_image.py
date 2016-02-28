#!/usr/bin/env python3
"""
Extract bitmap data and color attributes from standard input .prg
file.  This is only for hires bitmap .prg files, built with HermiRES.

"""
import sys
import os

def read_bitmap_and_color():
    f = sys.stdin.buffer
    f.seek(2)   # skip load address
    bitmap = bytearray(f.read(8000))
    color  = bytearray(f.read(1000))
    return (bitmap, color)

def extract_bitmap(bitmap, w, h):
    res = bytearray()
    for i in range(h):
        base = i * 40 * 8
        res.extend(bitmap[base : base + (w * 8)])
    return res

def extract_color(color, w, h):
    res = bytearray()
    for i in range(h):
        base = i * 40
        res.extend(color[base : base + w])
    return res

def main():
    if len(sys.argv) != 3:
        print("Usage: %s W H" % __file__)
        sys.exit(1)

    _, w, h = sys.argv
    w = int(w); h = int(h)

    bitmap, color = read_bitmap_and_color()
    image_bitmap = extract_bitmap(bitmap, w, h)
    image_color = extract_color(color, w, h)

    sys.stdout.buffer.write(image_bitmap)
    sys.stdout.buffer.write(image_color)

if __name__ == "__main__": main()
