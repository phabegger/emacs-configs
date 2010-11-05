#!/usr/bin/python

from optparse import OptionParser, OptionGroup
import os
import sys
import urllib
import urllib2

try:
    import simplejson as json
except ImportError:
    import json


VERSION = '1.0a1'
LOGIN_URL = 'https://xtrazone.sso.bluewin.ch/index.php/22,39,ajax_json,,,157/'
SMS_URL = 'https://xtrazone.sso.bluewin.ch/index.php/20,53,ajax,,,283/' + \
    '?route=%2Fmessaging%2Foutbox%2Fsendmobilemsg'


class MainCommand(object):
    '''Send SMS with the xtra gateway.

    '''

    def __call__(self):
        self.actions = []
        self._initialize_optparse()
        action_method = self._parse_and_validate()
        action_method()

    def _initialize_optparse(self):
        """Set up optparse with options
        """

        self.parser = OptionParser(version=VERSION,
                                   description=MainCommand.__doc__,
                                   usage='%progn ACTION [options]')
        self.parser.add_option('-d', '--captcha-temp-dir',
                               dest='captcha_temp_dir',
                               help='Captcha temp directory',
                               default='.')

        # login
        login = OptionGroup(self.parser, 'login action options')
        self.parser.add_option_group(login)
        login.add_option('-u', '--username', dest='username',
                         help='Username, used in combination with ' + \
                             '--login')
        login.add_option('-p', '--password', dest='password',
                         help='Password, used with --username and ' + \
                             '--login')
        self.actions.append((
                'login',
                'Login with --username and --password, which ' + \
                    'generates a token with a captcha. The ' + \
                    'captcha is downloaded and stored in a temp ' +\
                    'file, token and captcha-path is printed.',
                self.login,
                login))

        # send
        send = OptionGroup(self.parser, 'send action options')
        self.parser.add_option_group(send)
        send.add_option('-n', '--number', dest='number',
                        help='Receivesr mobile phone number in form ' +\
                            '+41 79 123 45 67',)
        send.add_option('-m', '--message', dest='message',
                        help='Message to send to the receiver.')
        send.add_option('-c', '--captcha', dest='captcha',
                        help='Text of the captcha.')
        send.add_option('-t', '--token', dest='token',
                        help='The token of the session')
        self.actions.append((
                'send',
                'Sends the message (--message) to the receiver ' + \
                    '(--number). The token (--token) has ' + \
                    'to be passed again with the text read from ' + \
                    'the captcha (--captcha).',
                self.send,
                send))

    def _parse_and_validate(self):
        """Parse options and validate them. If valid, it returns the
        action method.
        """

        self._extend_usage(self.parser)
        self.opts, self.args = self.parser.parse_args()
        action_mapping = dict([(a[0], a[1:]) for a in self.actions])
        if len(self.args) == 0 or self.args[0] not in action_mapping.keys():
            self._parse_error('A action (either login or send) should ' + \
                                  'be used.')

        action_data = action_mapping[self.args[0]]

        # options of the used action are required
        for opt in action_data[2].option_list:
            if not getattr(self.opts, opt.dest):
                self._parse_error('You need to use %s with the action %s' % (
                        opt._long_opts[0],
                        self.args[0]))

        # XXX patch
        self.opts.captcha_temp_dir = os.path.expanduser(
            self.opts.captcha_temp_dir)

        return action_data[1]

    def _extend_usage(self, parser):
        """Extend the optparse usage with actions.
        """

        usage = parser.get_usage()
        usage += '\nactions:\n\n'
        for name, desc, fun, group in self.actions:
            usage += '  %s: %s\n' % (
                name.ljust(10), desc)
        parser.set_usage(usage)

    def _parse_error(self, msg):
        self.parser.print_help()
        print '\n'
        print 'ERROR:', msg
        sys.exit(0)

    def login(self):
        """Logs in with --username and --password and prints token
        and path to captcha image.
        """

        # login request
        data = {
            'action': 'getCaptcha',
            'token': '',
            'do_sso_login': '0',
            'sso_user': self.opts.username,
            'sso_password': self.opts.password,
            'passphrase': ''}

        headers = {
            'Accept': 'application/json, text/javascript, */*',
            'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
            'Origin': 'https://xtrazone.sso.bluewin.ch',
            'Referer': 'https://xtrazone.sso.bluewin.ch/index.html.de',
            'User-Agent': 'Mozilla/5.0 (Macintosh; U; Intel Mac OS ' + \
                'X 10_6_4; de-de) AppleWebKit/533.18.1 (KHTML, like Gecko) ' +\
                'Version/5.0.2 Safari/533.18.5',
            'X-Header-Xtrazone': 'XtraZone',
            'X-Requested-With': 'XMLHttpRequest'
            }
        request = urllib2.Request(LOGIN_URL, urllib.urlencode(data), headers)
        response = urllib2.urlopen(request)

        # parse response, setup output
        jsondata = json.loads(response.read())
        token = jsondata.get('token')

        # prepare captcha file name
        captcha_file_path = os.path.abspath(os.path.join(
                self.opts.captcha_temp_dir, '%s.png' % token))

        # write some infos for the next request into a json file
        session_file = open(os.path.join(self.opts.captcha_temp_dir,
                                         '%s.json' % token), 'w')
        session_file.write(json.dumps({
                    'captcha_file': captcha_file_path,
                    'formdata': {
                        'action': 'ssoLogin',
                        'token': token,
                        'sso_user': self.opts.username,
                        'sso_password': self.opts.password,
                        'do_sso_login': 1,
                        'passphrase': ''
                        }}))
        session_file.close()

        # get the captcha image
        img_url = 'https:' + jsondata.get('img')
        request = urllib2.Request(img_url)
        response = urllib2.urlopen(request)

        captcha_file = open(captcha_file_path, 'wb')
        captcha_file.write(response.read())
        captcha_file.close()

        # print output
        print json.dumps({'token': jsondata.get('token'),
                          'captcha': captcha_file_path})

    def send(self):
        """Sends a sms
        """

        # get the session data
        session_file_path = os.path.join(self.opts.captcha_temp_dir,
                                         '%s.json' % self.opts.token)
        session_file = open(session_file_path)
        session_data = json.loads(session_file.read())
        session_file.close()

        # get / update formdata
        formdata = session_data.get('formdata')
        formdata['passphrase'] = self.opts.captcha

        # make the login request
        headers = {
            'Accept': 'application/json, text/javascript, */*',
            'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
            'Origin': 'https://xtrazone.sso.bluewin.ch',
            'Referer': 'https://xtrazone.sso.bluewin.ch/index.html.de',
            'User-Agent': 'Mozilla/5.0 (Macintosh; U; Intel Mac OS ' + \
                'X 10_6_4; de-de) AppleWebKit/533.18.1 (KHTML, like Gecko) ' +\
                'Version/5.0.2 Safari/533.18.5',
            'X-Header-Xtrazone': 'XtraZone',
            'X-Requested-With': 'XMLHttpRequest'
            }

        request = urllib2.Request(LOGIN_URL, urllib.urlencode(formdata),
                                  headers)
        response = urllib2.urlopen(request)
        jsonresp = json.loads(response.read())
        if jsonresp.get('status') == 'captcha_failed':
            print 'captcha failed'
            sys.exit(0)

        captcha_file = session_data.get('captcha_file')
        os.remove(captcha_file)
        os.remove(session_file_path)

        # make the sms send request
        data = {'receiversnames': self.opts.number,
                'recipients': '[]',
                'messagebody': self.opts.message,
                'attachments': '',
                'attachmentId':''}

        headers = {
            'Accept': 'application/json, text/javascript, */*',
            'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
            'Origin': 'https://xtrazone.sso.bluewin.ch',
            'Referer': 'https://xtrazone.sso.bluewin.ch/index.php/20?' + \
                'route=%2Fmessaging%2Findex%2Fnewmobilemsg',
            'User-Agent': 'Mozilla/5.0 (Macintosh; U; Intel Mac OS ' + \
                'X 10_6_4; de-de) AppleWebKit/533.18.1 (KHTML, like Gecko) ' +\
                'Version/5.0.2 Safari/533.18.5',
            'X-Header-Xtrazone': 'XtraZone',
            'X-Requested-With': 'XMLHttpRequest',
            'Cookie': self._extract_cookies(response),
            }

        request = urllib2.Request(SMS_URL, urllib.urlencode(data),
                                  headers)
        response = urllib2.urlopen(request)
        jsondata = json.loads(response.read())

        print jsondata.get('content')

    def _extract_cookies(self, response):
        """Extracts the cookies from the response
        """

        # extract cookies for next request
        cookie_headers = filter(lambda hd: hd.startswith('Set-Cookie:'),
                                response.headers.headers)
        cookies = [hd.split(': ')[1].split(';')[0]
                   for hd in cookie_headers]
        return ';'.join(cookies)


if __name__ == '__main__':
    MainCommand()()
