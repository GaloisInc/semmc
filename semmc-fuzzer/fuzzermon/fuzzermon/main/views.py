from django.http import HttpResponse
from django.http import JsonResponse
from django.shortcuts import render
import json

class Batch(object):
    def __init__(self):
        self.fuzzer_host = None
        self.fuzzer_user = None
        self.testing_host = None
        self.entries = []

    def __str__(self):
        return "batch"

class BatchEntry(object):
    def __init__(self):
        self.type = None
        self.count = None
        self.opcode = None
        self.operands = None
        self.state_values = []

class StateValue(object):
    def __init__(self, loc, exp, act):
        self.location = loc
        self.expected = exp
        self.actual = act

def parse_batch_json(body):
    raw = json.loads(body)
    b = Batch()

    b.fuzzer_host = raw['fuzzer-host']
    b.fuzzer_user = raw['fuzzer-user']
    b.testing_host = raw['testing-host']

    b.entries = []
    for entry in raw['entries']:
        be = BatchEntry()
        be.type = entry['type']
        be.opcode = entry['opcode']

        if be.type == 'success':
            be.count = int(entry['count'])
        elif be.type == 'failure':
            be.operands = entry['operands']

            for sval in entry['state']:
                be.state_values.append(StateValue(sval['location'], sval['expected'], sval['actual']))
        else:
            raise ValueError("Invalid batch entry type: %s" % (be.type,))

        b.entries.append(be)

    return b

def upload_batch(request):
    success = True

    try:
        batch = parse_batch_json(request.body.decode('utf-8'))
    except TypeError as e:
        msg = "Type error: %s" % (e,)
        success = False
    except KeyError as e:
        msg = "Invalid key %s" % (e,)
        success = False
    except ValueError as e:
        msg = "Invalid value: %s" % (e,)
        success = False

    resp = {}
    if success:
        resp['type'] = 'success'
    else:
        resp['type'] = 'failure'
        resp['message'] = msg

    return JsonResponse(resp)
