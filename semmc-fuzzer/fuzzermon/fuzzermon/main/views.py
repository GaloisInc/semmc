from django.http import HttpResponse
from django.http import JsonResponse
from django.shortcuts import render
import json

from main.models import *

class BatchData(object):
    def __init__(self):
        self.fuzzer_host = None
        self.fuzzer_user = None
        self.testing_host = None
        self.arch = None
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
    b = BatchData()

    b.fuzzer_host = raw['fuzzer-host']
    b.fuzzer_user = raw['fuzzer-user']
    b.testing_host = raw['testing-host']
    b.arch = raw['arch']

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
        # Parse the uploaded test result batch.
        batch = parse_batch_json(request.body.decode('utf-8'))

        # Insert the batch data into the database.
        # Create/load arch
        try:
            arch = Arch.objects.get(name=batch.arch)
        except Arch.DoesNotExist:
            arch = Arch(name=batch.arch)
            arch.save()

        # Create/load host
        try:
            testing_host = Host.objects.get(hostname=batch.testing_host)
        except Host.DoesNotExist:
            testing_host = Host(hostname=batch.testing_host, arch=arch)
            testing_host.save()

        # Create/load user
        try:
            user = User.objects.get(username=batch.fuzzer_user)
        except User.DoesNotExist:
            user = User(username=batch.fuzzer_user)
            user.save()

        # Create batch
        b = Batch()
        b.testing_host = testing_host
        b.fuzzer_host = batch.fuzzer_host
        b.user = user
        b.save()

        # Add entries
        for entry in batch.entries:
            if entry.type == 'success':
                e = TestSuccess()
                e.count = entry.count
                e.opcode = entry.opcode
                e.batch = b
                e.save()
            elif entry.type == 'failure':
                e = TestFailure()
                e.opcode = entry.opcode
                e.arguments = entry.operands
                e.batch = b
                e.save()

                for sve in entry.state_values:
                    sv = TestFailureState()
                    sv.test_failure = e
                    sv.location = sve.location
                    sv.expected_value = sve.expected
                    sv.actual_value = sve.actual
                    sv.save()
            else:
                raise Exception("BUG: Invalid entry type %s" % (entry.type,))

    except TypeError as e:
        msg = "Type error: %s" % (e,)
        success = False
    except KeyError as e:
        msg = "Invalid key %s" % (e,)
        success = False
    except ValueError as e:
        msg = "Invalid value: %s" % (e,)
        success = False
    except Exception as e:
        msg = "Error: %s" % (e,)
        success = False

    resp = {}
    if success:
        resp['type'] = 'success'
    else:
        resp['type'] = 'failure'
        resp['message'] = msg

    return JsonResponse(resp)

def index(request):
    allBatches = Batch.objects.all()
    context = {
            'batches': allBatches,
            }

    return render(request, 'main/index.html', context)

def view_batch(request, batch_id):
    b = Batch.objects.get(pk=batch_id)
    context = {
            'batch': b
            }
    return render(request, 'main/view_batch.html', context)
