# Compares a tracelog from FCEUX with our own logs made with cmplog
# If the code you want to compare is at the start of the init or play routine, be sure to clear the regs in FCEUX before starting.
import re

TANDYNSF_LINE = re.compile(r"([0-9A-F]{4}): A=([0-9A-F]{2}) X=([0-9A-F]{2}) Y=([0-9A-F]{2}) S=([0-9A-F]{4}) P=([Nn][Vv][Uu][Bb][Dd][Ii][Zz][Cc])")
FCEUX_LINE = re.compile(r"\$([0-9A-F]{4}):.+A:([0-9A-F]{2}) X:([0-9A-F]{2}) Y:([0-9A-F]{2}) S:([0-9A-F]{2}) P:([Nn][Vv][Uu][Bb][Dd][Ii][Zz][Cc])")

def main():
    tandynsf = []
    with open('6502log.txt', 'r') as infile:
        for line in infile:
            matches = TANDYNSF_LINE.match(line)
            if matches is None:
                raise Exception("Something went wrong reading TandyNSF log")
            PC = int(matches.group(1), 16)
            A = int(matches.group(2), 16)
            X = int(matches.group(3), 16)
            Y = int(matches.group(4), 16)
            S = int(matches.group(5), 16)
            P = matches.group(6)
            tandynsf.append((PC, A, X, Y, S, P))

    fceux = []
    with open('fceux.log', 'r') as infile:
        for line in infile:
            matches = FCEUX_LINE.match(line)
            if matches is None:
                continue
            PC = int(matches.group(1), 16)
            A = int(matches.group(2), 16)
            X = int(matches.group(3), 16)
            Y = int(matches.group(4), 16)
            S = int(matches.group(5), 16) + 0x102   # FCEUX's NSF player doesn't start with stack at FF
            P = matches.group(6)
            fceux.append((PC, A, X, Y, S, P))

    for i in xrange(len(tandynsf)):
        if tandynsf[i] != fceux[i]:
            print "First difference found on line", i+1
            print "TandyNSF: PC={0:04X}, A={1:02X}, X={2:02X}, Y={3:02X}, S={4:02X}, P={5}".format(*tandynsf[i])
            print "FCEUX:    PC={0:04X}, A={1:02X}, X={2:02X}, Y={3:02X}, S={4:02X}, P={5}".format(*fceux[i])
            break
    else:
        print "No differences found"

if __name__ == '__main__':
    main()
