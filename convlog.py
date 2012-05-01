# Converts an x86 tracelog from DosBox into a crude 6502 tracelog
import re

LINE = re.compile(r'EAX:([0-9A-F]{8}).+EDX:([0-9A-F]{8}).+ESI:([0-9A-F]{8}).+EDI:([0-9A-F]{8})')

def main():
    run6502 = raw_input("Enter address of Run6502 (1234:0000abcd): ")
    run6502 = run6502.upper()
    with open('LOGCPU.TXT', 'r') as infile:
        with open('6502log.txt', 'w') as outfile:
            for line in infile:
                if not line.startswith(run6502):
                    continue
                matches = LINE.search(line)
                if matches is None:
                    raise Exception("Something went wrong")
                eax = int(matches.group(1), 16)
                edx = int(matches.group(2), 16)
                esi = int(matches.group(3), 16)
                edi = int(matches.group(4), 16)
                a = eax & 0xff
                p = convflags((eax & 0xff00) >> 8)
                x = edx & 0xff
                y = (edx & 0xff00) >> 8
                outfile.write(
                    "{0:04X}: A={1:02X} X={2:02X} Y={3:02X} S={4:04X} P={5}\n".format(
                        edi, a, x, y, esi, p
                    )
                )

# Converts a value like 0x82 to flags like NvubdiZc
def convflags(value):
    flags = list('nvubdizc')
    for i in xrange(8):
        if value & (1 << i):
            flags[7-i] = flags[7-i].upper()
    return "".join(flags)

if __name__ == '__main__':
    main()
