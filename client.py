import sys
import cmd
import xmlrpclib
from pprint import pprint as pp


class RemoteReplClientCLI(cmd.Cmd):
    DEFAULT_PROMPT = "(RemoteREPL) "

    def _set_prompt(self):
        """Tries to set the prompt using the remote proc_name, falling
        back to some sane default
        """
        try:
            self.prompt = "(%s) " % self.proxy.proc_name()
        except xmlrpclib.Fault:
            self.prompt = self.DEFAULT_PROMPT
            pass

    def _set_server(self, hoststr):
        """Set/change the XML RPC server address by making a new
        ServerProxy instance
        """
        self.proxy = xmlrpclib.ServerProxy(hoststr)

    def __init__(self):
        cmd.Cmd.__init__(self, completekey = "Tab")
        self._set_server("http://localhost:4356/")
        self._set_prompt()

    def do_EOF(self,parameters):
        #TODO: Send stop command to proxy?
        print
        sys.exit()

    def completedefault(self, text, line, begidx, endidx):
        print "in complete default"
        return ['a']

    def precmd(self, line):
        if line == "EOF":
            # Don't tamper with EOF
            return line
        elif line.startswith("\\"):
            # Lines starting with back-slash are not eval'ifed
            return line[1:]
        else:
            # Any other lines have eval prefixed, so they are run via
            # the do_eval method
            return "eval %s" % line

    def do_eval(self, line):
        ret = self.proxy.eval(line)
        if ret['status'] == 'okay':
            print '[Executed okay]\n%s' % ret['outstr']
        elif ret['status'] == 'exception':
            print '[Traceback]\n%s' % ret['outstr']
        else:
            print "[Unknown response]"
            pp(ret)

        self._set_prompt()



p = RemoteReplClientCLI()
p.cmdloop("Remote REPL")
