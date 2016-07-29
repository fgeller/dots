import re
import os

def authinfo(login):
    p = re.compile('machine (\S+) login %s password (\S+) port (\S+)' % login)
    info = os.popen('/usr/local/bin/gpg -q --no-tty -d ~/.authinfo.gpg').read()
    matched = p.search(info)

    if matched:
        return matched.group(2)
    else:
        return None
