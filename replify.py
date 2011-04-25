import os
import sys
import StringIO
import threading
from SimpleXMLRPCServer import SimpleXMLRPCServer


class DefaultRemoteRepl(object):
    CMD_LOCK = threading.Lock()

    def proc_name(self):
        """Returns a useful, short description of where this is
        running (by default just the process ID)

        Used to format the REPL prompt, usually surrounded in parens, e.g:

        (1234) >>> print 'blah'
        """
        return "%s" % os.getpid()

    def list_locals(self):
        """Returns a list of locals, used for tab-completion
        """
        return locals().keys()

    def _belocked_run_with_result(self, cmdstr):
        # Switch out stdout/stderr so it can be captured
        new_out = StringIO.StringIO()
        save_stdout, save_stderr = sys.stdout, sys.stderr
        sys.stdout, sys.stderr = new_out, new_out

        try:
            try:
                # Try to eval as expression (e.g 1+4)
                expr_outstr = repr(eval(cmdstr, globals()))
            except SyntaxError:
                # syntax error is raised if non-expression is specified
                expr_outstr = None

                # Execute string
                exec cmdstr in globals()

        except Exception:
            # Exception, return stdout/stderr, followed by the traceback
            import traceback
            outstr = new_out.getvalue()
            tb = traceback.format_exc()
            outstr += "\n" + tb
            return {"status": "exception", "outstr": outstr}

        else:
            # Command executed without error, so return stdout/stderr, and
            # if it was expression, the return value of that
            outstr = new_out.getvalue()
            if expr_outstr is not None:
                if len(outstr) > 0 and not outstr.endswith("\n"):
                    outstr += "\n"
                outstr += expr_outstr
            return {"status": "okay", "outstr": outstr}

        finally:
            sys.stdout, sys.stderr = save_stdout, save_stderr

    def _run_with_result(self, cmdstr):
        self.CMD_LOCK.acquire()
        try:
            return self._belocked_run_with_result(cmdstr)
        finally:
            self.CMD_LOCK.release()

    def evalcmd(self, cmdstr):
        """Run the given command string, returning a dictionary with a
        key of status, and an optional "outstr" output string

        Possibly statuses are "unknown", "okay" or "exception"
        """
        retvalue = self._run_with_result(cmdstr)
        if retvalue is None:
            return {"status": "unknown"}
        return retvalue


class NukeRemoteRepl(DefaultRemoteRepl):
    def evalcmd(self, cmdstr):
        import nuke
        retvalue = nuke.executeInMainThreadWithResult(self._run_with_result, kwargs = {'cmdstr': cmdstr})
        if retvalue is None:
            return {"status": "unknown"}
        return retvalue


def go(bgthread = False):
    server = SimpleXMLRPCServer(("localhost", 4356))
    server.register_introspection_functions()

    try:
        import nuke
        if hasattr(nuke, 'env'):
            thingy = NukeRemoteRepl()
        else:
            raise ImportError("Probably not Nuke")
    except ImportError:
        thingy = DefaultRemoteRepl()

    server.register_function(thingy.evalcmd, 'eval')
    server.register_function(thingy.proc_name, 'proc_name')

    if bgthread:
        import threading
        print "Running server in thread"
        t = threading.Thread(target = server.serve_forever)
        t.setDaemon(True)
        t.start()
        return t
    else:
        print "Serving in foreground"
        server.serve_forever()

if __name__ == '__main__':
    go(bgthread = False)
