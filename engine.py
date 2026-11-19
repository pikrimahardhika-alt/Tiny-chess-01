import json,os,sys,urllib.error,urllib.request

URL=os.environ.get('MAKEFUN_ENV_URL') or sys.exit('MAKEFUN_ENV_URL is not set')
VAL={'p':100,'n':320,'b':330,'r':500,'q':900,'k':0}
ORTH=(1,-1,16,-16)
DIAG=(15,17,-15,-17)
ALL=ORTH+DIAG
KN=(33,31,18,14,-14,-18,-31,-33)
KING=ALL
LIMIT=50000
MATE=30000
B=[]
K=[0,0]
turn=True
rights=''
ep=-1
half=0
nodes=0
TT={}

class Stop(Exception): pass

def post(path,body):
    req=urllib.request.Request(URL+path,data=json.dumps(body).encode(),headers={'content-type':'application/json'})
    try:
        with urllib.request.urlopen(req,timeout=15) as r: return json.load(r)
    except urllib.error.HTTPError as e: sys.exit('%s: HTTP %s %s'%(path,e.code,e.read().decode(errors='replace')))

def setup(fen):
    global B,K,turn,rights,ep,half
    z=fen.split(); B=['.']*128; K=[0,0]
    for rr,row in enumerate(z[0].split('/')):
        f=0; r=7-rr
        for c in row:
            if c.isdigit(): f+=int(c)
            else:
                s=r*16+f; B[s]=c
                if c=='K': K[0]=s
                elif c=='k': K[1]=s
                f+=1
    turn=z[1]=='w'; rights=z[2] if z[2]!='-' else ''
    ep=(ord(z[3][0])-97)+16*(int(z[3][1])-1) if z[3]!='-' else -1
    half=int(z[4]) if len(z)>4 else 0

def attacked(s,w):
    for t in ((s-15,s-17) if w else (s+15,s+17)):
        if not t&0x88 and B[t]==('P' if w else 'p'): return True
    for d in KN:
        t=s+d
        if not t&0x88 and B[t]==('N' if w else 'n'): return True
    for d in KING:
        t=s+d
        if not t&0x88 and B[t]==('K' if w else 'k'): return True
    for i,d in enumerate(ALL):
        t=s+d
        while not t&0x88:
            p=B[t]
            if p!='.':
                if p.isupper()==w and (p.lower()=='q' or (i<4 and p.lower()=='r') or (i>=4 and p.lower()=='b')): return True
                break
            t+=d
    return False

def gen():
    us=turn
    for s in range(128):
        if s&0x88: continue
        p=B[s]
        if p=='.' or p.isupper()!=us: continue
        c=p.lower()
        if c=='p':
            d=16 if us else -16; r=s>>4; t=s+d
            if not t&0x88 and B[t]=='.':
                if t>>4 in (0,7):
                    for q in 'qrbn': yield (s,t,q)
                else:
                    yield (s,t,'')
                    if r==(1 if us else 6) and B[t+d]=='.': yield (s,t+ d,'')
            for x in (d-1,d+1):
                t=s+x
                if t&0x88: continue
                if (B[t]!='.' and B[t].isupper()!=us) or t==ep:
                    if t>>4 in (0,7):
                        for q in 'qrbn': yield (s,t,q)
                    else: yield (s,t,'')
        else:
            ds=KN if c=='n' else DIAG if c=='b' else ORTH if c=='r' else ALL if c=='q' else KING
            slide=c in 'bqr'
            for d in ds:
                t=s+d
                while not t&0x88:
                    if B[t]=='.': yield (s,t,'')
                    else:
                        if B[t].isupper()!=us: yield (s,t,'')
                        break
                    if not slide: break
                    t+=d
            if c=='k':
                enemy=not us
                for q,d in (('K' if us else 'k',1),('Q' if us else 'q',-1)):
                    if q in rights:
                        rook=s+(3 if d>0 else -4); t=s+2*d
                        if B[rook]==('R' if us else 'r') and all(B[s+i*d]=='.' for i in range(1,3 if d>0 else 4)) and all(not attacked(s+i*d,enemy) for i in range(3)):
                            yield (s,t,'')

def push(m):
    global turn,rights,ep,half
    a,b,q=m; us=turn; p=B[a]
    cs=b; cap=B[b]; d=16 if us else -16
    if p.lower()=='p' and b==ep and (a&7)!=(b&7): cs=b-d; cap=B[cs]; B[cs]='.'
    old=(p,cap,b,ep,rights,half,K[0],K[1],turn,cs)
    B[a]='.'; B[b]=(q.upper() if us else q) if q else p
    if p.lower()=='k':
        K[0 if us else 1]=b
        rights=rights.replace('K','').replace('Q','') if us else rights.replace('k','').replace('q','')
        if abs(b-a)==2:
            if b>a: B[a+1]=B[a+3]; B[a+3]='.'
            else: B[a-1]=B[a-4]; B[a-4]='.'
    if p.lower()=='r':
        for sq,ch in ((0,'Q'),(7,'K'),(112,'q'),(119,'k')):
            if a==sq: rights=rights.replace(ch,'')
    if cap.lower()=='r':
        for sq,ch in ((0,'Q'),(7,'K'),(112,'q'),(119,'k')):
            if cs==sq: rights=rights.replace(ch,'')
    ep=(a+b)//2 if p.lower()=='p' and abs(b-a)==32 else -1
    half=0 if p.lower()=='p' or cap!='.' else half+1
    turn=not us
    return old

def pop(m,st):
    global turn,rights,ep,half
    a,b,q=m; p,cap,_,ep,rights,half,K[0],K[1],turn,cs=st
    us=turn
    if p.lower()=='k' and abs(b-a)==2:
        if b>a: B[a+3]=B[a+1]; B[a+1]='.'
        else: B[a-4]=B[a-1]; B[a-1]='.'
    B[a]=p
    B[b]='.' if cs!=b else cap
    if cs!=b: B[cs]=cap
    K[0]=st[6]; K[1]=st[7]; turn=st[8]

def legal():
    us=turn
    for m in gen():
        st=push(m); ok=not attacked(K[0 if us else 1],not us); pop(m,st)
        if ok: yield m

def mate():
    return attacked(K[0 if turn else 1],not turn) and not any(legal())

def value(p): return VAL.get(p.lower(),0) if p!='.' else 0

def order(m):
    a,b,q=m; cap=B[b]
    if B[a].lower()=='p' and b==ep and (a&7)!=(b&7): cap='p'
    return value(cap)*10-value(B[a])+(value(q)*8 if q else 0)

def evaluate():
    x=phase=kw=kb=0
    for s in range(128):
        if s&0x88: continue
        p=B[s]
        if p=='.': continue
        c=p.lower(); f=s&7; r=s>>4; mid=12-abs(f*2-7)-abs(r*2-7)
        v=value(p)
        if c=='p': v+=(r if p.isupper() else 7-r)*8+(7-abs(f*2-7))*2
        elif c in 'nb': v+=mid*4
        elif c=='r': v+=8 if r==(6 if p.isupper() else 1) else 0
        elif c=='k':
            if p.isupper(): kw=mid
            else: kb=mid
        if c in 'qrbn': phase+=value(p)
        x+=v if p.isupper() else -v
    x+=(kw-kb)*(2 if phase<3500 else -2)
    return x if turn else -x

def qs(a,b,ply,qdepth):
    global nodes
    nodes+=1
    if nodes>LIMIT: raise Stop()
    check=attacked(K[0 if turn else 1],not turn)
    stand=evaluate()
    if not check:
        if stand>=b: return b
        if stand>a: a=stand
        if qdepth>=5: return a
    ms=[]
    for m in legal():
        if check or B[m[1]]!='.' or m[2] or (B[m[0]].lower()=='p' and m[1]==ep and (m[0]&7)!=(m[1]&7)): ms.append(m)
    if check and not ms: return -MATE+ply
    ms.sort(key=order,reverse=True)
    for m in ms:
        st=push(m)
        try: v=-qs(-b,-a,ply+1,qdepth+1)
        finally: pop(m,st)
        if v>=b: return b
        if v>a: a=v
    return a

def ttkey(ply):
    board=''.join(B[r*16+f] for r in range(8) for f in range(8))
    return board,turn,rights,ep,half,ply

def search(d,a,b,ply):
    global nodes
    nodes+=1
    if nodes>LIMIT: raise Stop()
    if half>=100:
        check=attacked(K[0 if turn else 1],not turn)
        return -MATE+ply if check and not any(legal()) else 0
    check=attacked(K[0 if turn else 1],not turn)
    if d<=0:
        if not check or ply>=8: return qs(a,b,ply,0)
        d=1
    elif check and ply<8: d+=1
    alpha=a; beta=b; k=ttkey(ply); ent=TT.get(k); ttmove=None
    if ent:
        td,v,flag,ttmove=ent
        if td>=d and (flag==0 or flag==1 and v<=a or flag==2 and v>=b): return v
    ms=list(legal())
    if not ms: return -MATE+ply if check else 0
    ms.sort(key=order,reverse=True)
    if ttmove in ms: ms.remove(ttmove); ms.insert(0,ttmove)
    best=-MATE; bm=None
    for m in ms:
        st=push(m)
        try: v=-search(d-1,-b,-a,ply+1)
        finally: pop(m,st)
        if v>best: best=v; bm=m
        if v>a: a=v
        if a>=b: break
    flag=1 if best<=alpha else 2 if best>=beta else 0
    if len(TT)<60000 or k in TT: TT[k]=(d,best,flag,bm)
    return best

def sq(s): return ord(s[0])-97+16*(int(s[1])-1)
def parse(u): return (sq(u[:2]),sq(u[2:4]),u[4].lower() if len(u)>4 else '')
def uci(m):
    a,b,q=m
    return chr(97+(a&7))+str((a>>4)+1)+chr(97+(b&7))+str((b>>4)+1)+q

def rootfilter(ms):
    safe=[]
    for m in ms:
        st=push(m); won=mate(); bad=False
        if not won:
            for r in legal():
                st2=push(r); bad=mate(); pop(r,st2)
                if bad: break
        pop(m,st)
        if won: return [m]
        if not bad: safe.append(m)
    return safe or ms

def choose(g):
    global nodes,TT
    setup(g['fen']); ms=[parse(x) for x in g['legal_moves']]
    if not ms: return g['legal_moves'][0]
    ms=rootfilter(ms); ms.sort(key=order,reverse=True); best=ms[0]; nodes=0; TT={}
    for depth in range(1,32):
        a=-MATE; pick=best; score=-MATE
        try:
            for m in ms:
                st=push(m)
                try: v=-search(depth-1,-MATE,-a,1)
                finally: pop(m,st)
                if v>score: score=v; pick=m
                if v>a: a=v
        except Stop: break
        best=pick
        ms.remove(best); ms.insert(0,best)
    return uci(best)

while True:
    g=post('/game',{})
    if g['status']=='done': break
    while g['status']=='your_move':
        g=post('/move',{'game':g['game'],'move':choose(g)})
    print('game %s: %s by %s'%(g['game'],g['outcome'],g['reason']),file=sys.stderr)
print('done: %s won, %s drawn, %s lost'%(g.get('wins'),g.get('draws'),g.get('losses')),file=sys.stderr)
