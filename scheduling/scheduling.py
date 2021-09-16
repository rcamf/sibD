from collections import namedtuple, defaultdict
from itertools import chain

schedule_input = 'r2(x) r1(y) w3(x) w2(x) r3(y) w3(y) w2(y) w2(z) a2 r1(z) w1(z) c1 w3(z) c3'

Op = namedtuple('Op', ['action', 'transaction', 'object'])


def string_op(op):
    if op.action in ['c', 'a']:
        return '{}{}'.format(op.action, op.transaction)
    else:
        return '{}{}({})'.format(*op)


Op.__repr__ = string_op


def parse(s_input):
    return [
        Op(op[0], int(op[1]), op[-2] if op[-2] != op[0] else '')
        for op in s_input.split()
    ]


def aborts(s):
    return [op.transaction for op in s if op.action == 'a']


def commits(s):
    return [op.transaction for op in s if op.action == 'c']


def conf(s):
    commited = [t for (x, t, _) in s if x == 'c']
    ops = [Op(x, t, o) for (x, t, o) in s if t in commited and x != 'c']

    confs = []
    for i, op in enumerate(ops):
        x, t, o = op
        conflicts = ['r', 'w'] if x == 'w' else ['w']
        confs += [(op, op2) for op2 in ops[i + 1:] if t != op2.transaction
                  and o == op2.object and op2.action in conflicts]
    return confs


def conf_equivalent(s1, s2):
    pass


def display_confgraph(s):
    import graphviz

    edges = {(op1.transaction, op2.transaction) for op1, op2 in conf(s)}
    nodes = {op.transaction for op in s}

    dot = graphviz.Digraph('Konfliktgraph')
    for node in nodes:
        dot.node(str(node))
    for (t1, t2) in edges:
        dot.edge(str(t1), str(t2))
    dot.render(view=True)


def reads(s):
    ops = list(reversed(s))
    reads = []
    for i, (x, t, o) in enumerate(ops):
        if x == 'r':
            aborted = aborts(s)
            write_t = next(
                (op.transaction for op in ops[i:]
                 if op.action == 'w' and op.transaction not in aborted
                 and op.transaction != t and op.object == o), None)
            if write_t is not None:
                reads.append((t, o, write_t))
    return list(reversed(reads))


def rc(s):
    s_reads = reads(s)
    commited = commits(s)
    for ti, x, tj in s_reads:
        print('{} liest {} von {}: '.format(ti, x, tj), end='')
        if ti in commited:
            if tj not in commited[:commited.index(ti)]:
                print('{} wird nicht vor {} commited. NON-RC.'.format(tj, ti))
                return False
            print('{} wird vor {} commited. OK.'.format(tj, ti))
        else:
            print('{} wird nicht commited. OK.'.format(ti))
    print('RC')
    return True


def aca(s):
    for ti, x, tj in reads(s):
        print('t{} liest {} von t{}: '.format(ti, x, tj), end='')
        index = s.index(Op('r', ti, x))
        if ('c', tj, '') not in s[:index]:
            print('t{} wird nicht vor r{}({}) commited. NON-ACA.'.format(
                tj, ti, x))
            return False
        print('{} wird vor r{}({}) commited. OK.'.format(tj, ti, x))
    print('ACA')
    return True


def st(s):
    commited = commits(s)
    for i, writeop in enumerate(s):
        (x, t, o) = writeop
        if x == 'w':
            print('Betrachte {}: '.format(writeop), end='')
            op_end = Op('c' if t in commited else 'a', t, '')
            for op in s[i:s.index(op_end)]:
                if op.transaction != t and op.object == o:
                    print('{} < {} aber {} < {}. NON-ST'.format(
                        writeop, op, op, op_end))
                    return False
            print(
                'auf {} wird vor {} von keinen anderen Transaktionen zugegriffen. OK.'
                .format(o, op_end))
    print('ST')
    return True


def lock(op):
    a, t, o = op
    return Op(a + 'l', t, o)
def unlock(op):
    a, t, o = op
    return Op(a + 'u', t, o)

def c2pl(s):
    ns = []
    delayed = []
    locks = {}
    remaining = s.copy()
    while remaining:
        op = remaining.pop(0)
        if op.action in ['a', 'c']:
            ns += [op]
        elif op.transaction not in locks:
            required = [(a + 'l', x) for a, t, x in s
                        if t == op.transaction and a not in ['c', 'a']]
            active = list(chain.from_iterable(locks.values()))
            for r in required:
                if r[0] == 'wl' and r[1] in [
                        o for a, o in active
                ] or r[0] == 'rl' and ('wl', r[1]) in active:
                    delayed.append(op)
                    break
            else:
                ns += [Op(a, op.transaction, o) for (a, o) in required]
                ns += [op, unlock(op)]
                # Lock for current action has already been released.
                locks[op.transaction] = required[1:]
                remaining = delayed + remaining
                delayed = []
        else:
            # Required locks have been aquired.
            ns += [op, unlock(op)]
            locks[op.transaction] = locks[op.transaction][1:]
            remaining = delayed + remaining
            delayed = []
    return ns


s = parse(schedule_input)
s2 = parse(
    'r3(y) r3(z) w2(x) r1(y) w1(x) r2(x) r2(z) w1(y) c1 w3(x) c3 w2(y) r3(x) c2'
)

def s2pl(s, strict=False):
    ns = []
    delayed = []
    locks = defaultdict(list)
    remaining = s.copy()
    while remaining:
        op = remaining.pop(0)
        a, t, o = op
        if a in ['a', 'c']:
            # print(op, remaining, delayed)
            if t in [dop.transaction for dop in delayed]:
                delayed.append(op)
            else:
                ns += [op]
                ns += [unlock(Op(la, t, lo)) for la, lo in locks[t]]
                locks[t] = []
                remaining = delayed + remaining
                delayed = []
        else:
            active = list(chain.from_iterable(locks.values()))
            if ('w', o) in active or (a == 'w' and ('r', o) in active) or t in [dop.transaction for dop in delayed]:
                delayed.append(op)
            else:
                ns += [lock(op), op]
                if not strict and a == 'r':
                    ns += [unlock(op)]
                else:
                    locks[t].append((a, o))
                remaining = delayed + remaining
                delayed = []
    if delayed:
        return False, ns, delayed
    else:
        return True, ns
