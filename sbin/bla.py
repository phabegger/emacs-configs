

class Tester(object):

    def __init__(self, context, request):
        self.context = context
        self.request = request

    def plone_reload(self, zcml=False):
        from Acquisition import aq_inner, aq_parent
        from OFS.interfaces import IApplication
        root = aq_parent(self.context)
        while aq_parent(root) and not IApplication.providedBy(root):
            root = aq_inner(aq_parent(root))
        # get reload view from plone.reload
        reload = root.restrictedTraverse('reload')
        if zcml:
            print reload.zcml_reload()
        else:
            print reload.code_reload()

    def __call__(self):
        date()
        sep()


# ---------------------------------------------------

RUN_METHOD = None
CALL_CLASS = Tester

# ---------------------------------------------------

import sys
import traceback
import datetime

class ExternalHelper(object):

    def __new__(cls, context):
        obj = object.__new__(cls)
        cls.__init__(obj)
        obj.__output = []
        obj.__prior_stdout = sys.stdout
        sys.stdout = obj
        request = None
        if context:
            request = context.REQUEST
        try:
            if RUN_METHOD:
                RUN_METHOD(context, request)
            elif CALL_CLASS:
                CALL_CLASS(context, request)()
            else:
                print '...specify method or class...'
        except:
            if __name__!='__main__':
                try:
                    import transaction
                    transaction.abort()
                except:
                    print '(could not abort transaction)'
            try:
                import transaction
                transaction.abort()
            except:
                print '(could not abort transaction)'
            try:
                exc = ''.join(traceback.format_exception(*sys.exc_info()))
                print exc
            except:
                pass
        return obj.__stop__()

    def write(self, string):
        self.__write__(string)

    def humanPrint(self, *args):
        for x in args:
            print self.__render(x)

    def __render(self, obj, i=0):
        """
        Human readable lists and dicts
        """
        if isinstance(obj, dict):
            return self.__render_dict(obj, i)
        elif isinstance(obj, list) or isinstance(obj, tuple):
            return self.__render_list(obj, i)
        elif isinstance(obj, str):
            return "%s'%s'" % ('\t' * i, obj)
        elif isinstance(obj, unicode):
            return "%su'%s'" % ('\t' * i, obj)
        else:
            return str(obj).replace('\n', '\n' + '\t' * i)

    def __render_dict(self, d, i=1):
        si = '\t' * i
        ret = [si + '{']
        for k,v in d.items():
            k = "'%s'" % str(k)
            k.ljust(30)
            if type(v) in [dict, tuple, list]:
                v = self.__render(v, i=i+1)
            else:
                v = self.__render(v, i=0)
            ret.append('%s%s : %s,' % ('\t' * (i+1), k, v))
        ret.append(si + '}')
        return '\n'.join(ret)

    def __render_list(self, l, i=1):
        si = '\t' * i
        ret = []
        for x in l:
            ret.append('%s,' % (self.__render(x, i=i+1)))
        if isinstance(l, tuple):
            ret = ['%s(' % si] + ret + ['%s)' % si]
        else:
            ret = ['%s[' % si] + ret + ['%s]' % si]
        return '\n'.join(ret)

    def __write__(self, string):
        self.__output.append(string)
        self.__prior_stdout.write(string)

    def __stop__(self):
        sys.stdout = self.__prior_stdout
        return ''.join(self.__output)


bla = lambda self=None:ExternalHelper(self)
x = lambda *a: sys.stdout.humanPrint(*a)
d = lambda *a: [sys.stdout.humanPrint(dir(x)) for x in a]
i = lambda *a: [sys.stdout.humanPrint(x.__dict__) for x in a]
sep = lambda c='-', l=100: sys.stdout.write('%s\n' % (c*l))
date = lambda:sys.stdout.write(str(datetime.datetime.now())+'\n')

def xd(obj):
    try:
        parent = obj.aq_inner.aq_parent
    except AttributeError:
        return d(obj)
    aa = list(set(dir(obj)) - set(dir(parent)))
    aa.sort()
    return sys.stdout.humanPrint(aa)

if __name__=='__main__':
    bla()

