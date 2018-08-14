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
        self.opcode = None
        self.pretty = None
        self.bytes = None
        self.operands = None
        self.state_values = []
        self.signal_num = None
        self.inputs = []
        self.passed = False

class StateValue(object):
    def __init__(self, loc, exp, act):
        self.location = loc
        self.expected = exp
        self.actual = act

class Input(object):
    def __init__(self, loc, val):
        self.location = loc
        self.value = val

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
        be.opcode = entry['opcode']
        be.operands = entry['raw-operands']
        be.pretty = entry['pretty']
        be.bytes = entry['bytes']

        for sval in entry['state']:
            be.state_values.append(StateValue(sval['location'], sval['expected'], sval['actual']))

        for ival in entry['inputs']:
            be.inputs.append(Input(ival['location'], ival['value']))

        if entry['type'] == 'success':
            be.passed = True
        elif entry['type'] == 'failure':
            be.passed = False
        elif entry['type'] == 'unexpectedSignal':
            be.passed = False
            be.signal_num = int(entry['signal'])
        else:
            raise ValueError("Invalid entry type: %s" % (entry['type'],))

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

            e = Test()
            e.pretty = entry.pretty
            e.opcode = opcode
            e.batch = b
            e.bytes = entry.bytes
            e.arguments = entry.operands
            e.passed = entry.passed
            e.signal = entry.signal_num

            e.save()

            TestMachineState.objects.bulk_create([
                TestMachineState(test=e, location=sve.location, expected_value=sve.expected, actual_value=sve.actual)
                for sve in entry.state_values ])

            TestInput.objects.bulk_create([
                TestInput(test=e, location=i.location, value=i.value)
                for i in entry.inputs ])

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

def arch_list(request):
    archs = Arch.objects.all()
    results = []

    for arch in archs:
        host_data = []
        for h in arch.host_set.all():
            all_batches = Batch.objects.filter(testing_host__id=h.id).order_by('-submitted_at') 
            if all_batches:
                last_batch_time = all_batches[0].submitted_at
            else:
                last_batch_time = None

            host_data.append({
                'host': h,
                'last_batch_time': last_batch_time,
                'num_failures': Test.objects.filter(batch__testing_host__id=h.id, passed=False).count(),
                'num_successes': Test.objects.filter(batch__testing_host__id=h.id, passed=True).count(),
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
        num_failures = Test.objects.filter(opcode__id=opcode.id, passed=False).count()
        num_successes = Test.objects.filter(opcode__id=opcode.id, passed=True).count()

        denom = num_failures + num_successes
        if denom == 0:
            percent = 100
        else:
            percent = round(100.0 * (num_failures / denom), 2)

        results = {
                'opcode': opcode,
                'num_failures': num_failures,
                'num_successes': num_successes,
                'percent_failing': percent,
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

def view_opcode(request, opcode_id, result_type='success'):
    # User clicked the 'delete all opcodes' button:
    if request.POST.get('delete-all'):
        Test.objects.filter(opcode__id=opcode_id).delete()

    set_display_mode(request)
    o = Opcode.objects.get(pk=opcode_id)

    try:
        limit_str = request.GET.get('limit')
        if limit_str == 'none':
            limit = None
        else:
            limit = int(request.GET.get('limit'))
    except:
        limit = 50

    cases = Test.objects.filter(opcode__id=o.id)

    successes = cases.filter(passed=True)
    failures = cases.filter(signal__isnull=True, passed=False)
    signals = cases.filter(signal__isnull=False)

    if result_type == 'success':
        cases = successes
    elif result_type == 'signal':
        cases = signals
    elif result_type == 'failure':
        cases = failures

    cases = cases.order_by('-batch__submitted_at')
    if limit:
        cases = cases[:limit]

    context = {
            'opcode': o,
            'cases': cases,
            'numty': request.session['numeric_display'],
            'limit': limit,
            'result_type': result_type,
            'num_successes': successes.count(),
            'num_signals': signals.count(),
            'num_failures': failures.count(),
            }

    return render(request, 'main/view_opcode.html', context)

def view_test(request, test_id):
    set_display_mode(request)
    tf = Test.objects.get(pk=test_id)

    context = {
            'failure': tf,
            'numty': request.session['numeric_display'],
            'inputs': tf.testinput_set.all(),
            }

    return render(request, 'main/view_test.html', context)

def set_display_mode(request):
    display_mode = request.GET.get('numeric_display') or request.session.get('numeric_display')
    display_modes = ['dec', 'bin', 'hex']

    if display_mode not in display_modes:
        display_mode = display_modes[0]

    request.session['numeric_display'] = display_mode
