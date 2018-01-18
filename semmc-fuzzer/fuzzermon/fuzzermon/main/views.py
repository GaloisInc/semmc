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
        self.pretty = None
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
            be.operands = entry['raw-operands']
            be.pretty = entry['pretty']

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
            try:
                opcode = Opcode.objects.get(name=entry.opcode, arch=arch)
            except Opcode.DoesNotExist:
                opcode = Opcode()
                opcode.name = entry.opcode
                opcode.arch = arch
                opcode.save()

            if entry.type == 'success':
                e = TestSuccess()
                e.count = entry.count
                e.opcode = opcode
                e.batch = b
                e.save()
            elif entry.type == 'failure':
                e = TestFailure()
                e.opcode = opcode
                e.pretty = entry.pretty
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

def arch_list(request):
    archs = Arch.objects.all()
    results = []

    for arch in archs:
        host_data = []
        for h in arch.host_set.all():
            host_data.append({
                'host': h,
                'num_failures': TestFailure.objects.filter(batch__testing_host__id=h.id).count(),
                'num_successes': TestSuccess.objects.filter(batch__testing_host__id=h.id).count(),
                })

        results.append({
            'arch': arch,
            'hosts': host_data,
                })

    context = {
            'archs': results
            }
    return render(request, 'main/index.html', context)

def view_arch(request, arch_id):
    sort_orders = {
            'opcode': lambda r: r['opcode'].name,
            'num_failures': lambda r: r['num_failures'],
            'num_successes': lambda r: r['num_successes'],
            'percent_failing': lambda r: r['percent_failing'],
            }
    sort_dir = request.GET.get('dir') or 'asc'

    sort_order = request.GET.get('sort')
    if sort_order not in sort_orders:
        sort_order = 'opcode'

    sort_order_func = sort_orders[sort_order]

    a = Arch.objects.get(pk=arch_id)

    opcodes = Opcode.objects.filter(arch__id=arch_id)
    opcode_results = []

    for opcode in opcodes:
        num_failures = TestFailure.objects.filter(opcode__id=opcode.id).count()
        num_successes = TestSuccess.objects.filter(opcode__id=opcode.id).count()

        results = {
                'opcode': opcode,
                'num_failures': num_failures,
                'num_successes': num_successes,
                'percent_failing': round(100.0 * (num_failures / (num_failures + num_successes)), 2),
                }
        opcode_results.append(results)

    opcode_results.sort(key=sort_order_func)

    if sort_dir == 'desc':
        opcode_results.reverse()

    flipdir = 'asc' if sort_dir == 'desc' else 'desc'

    columns = [
            { 'name': 'opcode', 'display_name': 'Opcode', },
            { 'name': 'num_failures', 'display_name': '# Failures', },
            { 'name': 'num_successes', 'display_name': '# Successes', },
            { 'name': 'percent_failing', 'display_name': '% Failing', },
            ]

    context = {
            'arch': a,
            'opcode_statuses': opcode_results,
            'flipdir': flipdir,
            'sort_order': sort_order,
            'cols': columns,
            }

    return render(request, 'main/view_arch.html', context)

def view_opcode(request, opcode_id):
    display_mode = request.GET.get('numeric_display') or request.session.get('numeric_display')
    display_modes = ['dec', 'bin', 'hex']

    if display_mode not in display_modes:
        display_mode = display_modes[0]

    request.session['numeric_display'] = display_mode

    o = Opcode.objects.get(pk=opcode_id)

    failures = TestFailure.objects.filter(opcode__id=o.id)

    context = {
            'opcode': o,
            'failures': failures,
            'numty': display_mode,
            }

    return render(request, 'main/view_opcode.html', context)
