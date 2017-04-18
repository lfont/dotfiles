"""
   Get account password from a password store.
"""
import subprocess

def get_output(cmd):
    # Bunch of boilerplate to catch the output of a command:
    pipe = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (output, errout) = pipe.communicate()
    assert pipe.returncode == 0 and not errout
    return output

"""
   http://www.emacswiki.org/emacs/OfflineIMAP#toc2
"""
def get_password_emacs(host, port = 993):
    cmd = "emacsclient -e '(authinfo-get-password \"%s\" \"%s\")'" % (host, port)
    return get_output(cmd).strip().lstrip('"').rstrip('"')

def get_password_pass(name):
    cmd = "pass show \"%s\"" % (name)
    return get_output(cmd).split('\n')[-2].split(':')[1].strip()
