import json,os,sys,urllib.request,urllib.error

V={'p':100,'n':320,'b':330,'r':500,'q':900,'k':0}
O=(1,-1,16,-16); D=(15,17,-15,-17); A=O+D
N=(33,31,18,14,-14,-18,-31,-33)
S=tuple(16*r+f for r in range(8) for f in range(8))
MATE=30000; LIMIT=50000
B=['.']*128; K=[4,116]; white=True; rights=''; ep=-1; hm=0
nodes=0; tt={}; hist={}; killers=[[] for _ in range(80)]; reps={}

def post(path,data):
    u=os.environ.get('MAKEFUN_ENV_URL')
    if not u: sys.exit('MAKEFUN_ENV_URL is not set')
    q=urllib.request.Request(u+path,data=json.dumps(data).encode(),headers={'content-type':'application/json'})
    try:
        with urllib.request.urlopen(q,timeout=15) as r:return json.load(r)
    except urllib.error.HTTPError as e:sys.exit('%s: HTTP %s %s'%(path,e.code,e.read().decode(errors='replace')))

def sq(x):return ord(x[0])-97+16*(int(x[1])-1)
def uci(m):
    a,b,p=m;return chr(97+(a&7))+str((a>>4)+1)+chr(97+(b&7))+str((b>>4)+1)+p
def parse(x):return sq(x[:2]),sq(x[2:4]),x[4].lower() if len(x)>4 else ''

def setup(f):
    global B,K,white,rights,ep,hm
    z=f.split();B=['.']*128;K=[4,116]
    for rr,row in enumerate(z[0].split('/')):
        c=0;r=7-rr
        for x in row:
            if x.isdigit():c+=int(x)
            else:
                t=16*r+c;B[t]=x
                if x=='K':K[0]=t
                if x=='k':K[1]=t
                c+=1
    white=z[1]=='w';rights='' if z[2]=='-' else z[2]
    ep=sq(z[3]) if z[3]!='-' else -1;hm=int(z[4]) if len(z)>4 else 0

def attacked(s,w):
    for t in ((s-15,s-17) if w else (s+15,s+17)):
        if not t&0x88 and B[t]==('P' if w else 'p'):return True
    for d in N:
        t=s+d
        if not t&0x88 and B[t]==('N' if w else 'n'):return True
    for d in A:
        t=s+d
        if not t&0x88 and B[t]==('K' if w else 'k'):return True
    for i,d in enumerate(A):
        t=s+d
        while not t&0x88:
            p=B[t]
            if p!='.':
                if p.isupper()==w and (p.lower()=='q' or i<4 and p.lower()=='r' or i>=4 and p.lower()=='b'):return True
                break
            t+=d
    return False

def pseudo(caps=False):
    us=white
    for a in S:
        p=B[a]
        if p=='.' or p.isupper()!=us:continue
        c=p.lower()
        if c=='p':
            d=16 if us else -16;t=a+d
            if not t&0x88 and B[t]=='.':
                if t>>4 in (0,7):
                    for q in 'qrbn':yield a,t,q
                elif not caps:
                    yield a,t,''
                    if a>>4==(1 if us else 6) and B[t+d]=='.':yield a,t+d,''
            for e in (d-1,d+1):
                t=a+e
                if t&0x88:continue
                if B[t]!='.' and B[t].isupper()!=us and B[t].lower()!='k' or t==ep:
                    if t>>4 in (0,7):
                        for q in 'qrbn':yield a,t,q
                    else:yield a,t,''
        else:
            ds=N if c=='n' else D if c=='b' else O if c=='r' else A
            slide=c in 'bqr'
            for d in ds:
                t=a+d
                while not t&0x88:
                    if B[t]=='.':
                        if not caps:yield a,t,''
                    else:
                        if B[t].isupper()!=us and B[t].lower()!='k':yield a,t,''
                        break
                    if not slide:break
                    t+=d
            if c=='k' and not caps and a==(4 if us else 116):
                enemy=not us
                for ch,d in ((('K' if us else 'k'),1),(('Q' if us else 'q'),-1)):
                    if ch in rights:
                        rook=a+(3 if d>0 else -4);t=a+2*d
                        if B[rook]==('R' if us else 'r') and all(B[a+i*d]=='.' for i in range(1,3 if d>0 else 4)) and all(not attacked(a+i*d,enemy) for i in range(3)):
                            yield a,t,''

def push(m):
    global white,rights,ep,hm
    a,b,q=m;us=white;p=B[a];cs=b;cap=B[b];d=16 if us else -16
    if p.lower()=='p' and b==ep and (a&7)!=(b&7):cs=b-d;cap=B[cs];B[cs]='.'
    old=(p,cap,cs,ep,rights,hm,K[0],K[1],white)
    B[a]='.';B[b]=(q.upper() if us else q) if q else p
    if p.lower()=='k':
        K[0 if us else 1]=b;rights=rights.replace('K','').replace('Q','') if us else rights.replace('k','').replace('q','')
        if abs(b-a)==2:
            x=a+3 if b>a else a-4;y=a+1 if b>a else a-1;B[y]=B[x];B[x]='.'
    for x,ch in ((0,'Q'),(7,'K'),(112,'q'),(119,'k')):
        if p.lower()=='r' and a==x or cap.lower()=='r' and cs==x:rights=rights.replace(ch,'')
    ep=(a+b)//2 if p.lower()=='p' and abs(b-a)==32 else -1
    hm=0 if p.lower()=='p' or cap!='.' else hm+1;white=not us
    return old

def pop(m,o):
    global white,rights,ep,hm
    a,b,q=m;p,cap,cs,ep,rights,hm,K[0],K[1],white=o
    if p.lower()=='k' and abs(b-a)==2:
        x=a+3 if b>a else a-4;y=a+1 if b>a else a-1;B[x]=B[y];B[y]='.'
    B[a]=p;B[b]='.' if cs!=b else cap
    if cs!=b:B[cs]=cap
    K[0]=o[6];K[1]=o[7];white=o[8]

def key():return ''.join(B[x] for x in S)+('w' if white else 'b')+rights+str(ep)
def legal(caps=False):
    us=white
    for m in pseudo(caps):
        o=push(m);ok=not attacked(K[0 if us else 1],not us);pop(m,o)
        if ok:yield m
def draw(ply):
    if hm>=100 or reps.get(key(),0)>=3:
        if attacked(K[0 if white else 1],not white) and not any(legal()):return -MATE+ply
        return 0
    return None

def evaluate():
    x=phase=wb=bb=kw=kb=0
    for s in S:
        p=B[s]
        if p=='.':continue
        c=p.lower();f=s&7;r=s>>4;mid=14-abs(2*f-7)-abs(2*r-7);v=V[c]
        if c=='p':v+=(r if p.isupper() else 7-r)*8+(7-abs(2*f-7))*2
        elif c in 'nb':v+=mid*(4 if c=='n' else 3)
        elif c=='r':v+=8 if r==(6 if p.isupper() else 1) else 0
        elif c=='k':
            if p.isupper():kw=mid
            else:kb=mid
        if c=='b':
            if p.isupper():wb+=1
            else:bb+=1
        if c in 'qrbn':phase+=v
        x+=v if p.isupper() else -v
    x+=(wb>1)*25-(bb>1)*25+(kw-kb)*(2 if phase<3500 else -2)
    return x if white else -x

def order(m,ply,tm):
    if m==tm:return 1000000
    a,b,q=m;v=B[b]
    if B[a].lower()=='p' and b==ep and a&7!=b&7:v='p'
    if v!='.':return 10000+V[v.lower()]*10-V[B[a].lower()]+(V[q]*8 if q else 0)
    if q:return 9000+V[q]*8
    if ply<len(killers) and m in killers[ply]:return 8000-killers[ply].index(m)*100
    return hist.get((a,b),0)

class Stop(Exception):pass
def tick():
    global nodes
    nodes+=1
    if nodes>LIMIT:raise Stop

def qsearch(a,b,ply,qd):
    tick()
    z=draw(ply)
    if z is not None:return z
    check=attacked(K[0 if white else 1],not white);stand=evaluate()
    if not check:
        if stand>=b:return b
        if stand>a:a=stand
        if qd>=8:return a
    elif qd>=10:
        ms=list(legal())
        if not ms:return -MATE+ply
        for m in ms:
            o=push(m);k=key();reps[k]=reps.get(k,0)+1
            try:
                z=draw(ply+1);v=-z if z is not None else -qsearch(-b,-a,ply+1,qd+1)
            finally:
                reps[k]-=1
                if not reps[k]:del reps[k]
                pop(m,o)
            if v>a:a=v
            if a>=b:return b
        return a
    ms=list(legal() if check else legal(True));ms.sort(key=lambda m:order(m,ply,None),reverse=True)
    if check and not ms:return -MATE+ply
    for m in ms:
        o=push(m);k=key();reps[k]=reps.get(k,0)+1
        try:
            z=draw(ply+1);v=-z if z is not None else -qsearch(-b,-a,ply+1,qd+1)
        finally:
            reps[k]-=1
            if not reps[k]:del reps[k]
            pop(m,o)
        if v>=b:return b
        if v>a:a=v
    return a

def search(d,a,b,ply):
    tick();k=key()
    z=draw(ply)
    if z is not None:return z
    check=attacked(K[0 if white else 1],not white)
    if d<=0:return qsearch(a,b,ply,0)
    if check and ply<12:d+=1
    tk=k+str(hm);alpha=a;ent=tt.get(tk);tm=None
    if ent:
        td,v,fl,tm=ent
        if td>=d and (fl==0 or fl==1 and v<=a or fl==2 and v>=b):return v
    ms=list(legal())
    if not ms:return -MATE+ply if check else 0
    ms.sort(key=lambda m:order(m,ply,tm),reverse=True);best=-MATE;bm=None
    for i,m in enumerate(ms):
        quiet=B[m[1]]=='.' and not m[2] and not (B[m[0]].lower()=='p' and m[1]==ep and m[0]&7!=m[1]&7)
        o=push(m);rk=key();reps[rk]=reps.get(rk,0)+1
        try:
            if i==0:v=-search(d-1,-b,-a,ply+1)
            else:
                red=d>=3 and i>=4 and quiet and not check
                v=-search(d-2 if red else d-1,-a-1,-a,ply+1)
                if v>a:v=-search(d-1,-b,-a,ply+1)
        finally:
            reps[rk]-=1
            if not reps[rk]:del reps[rk]
            pop(m,o)
        if v>best:best=v;bm=m
        if v>a:a=v
        if a>=b:
            if quiet:
                if ply<len(killers):
                    if m in killers[ply]:killers[ply].remove(m)
                    killers[ply].insert(0,m);del killers[ply][2:]
                hist[(m[0],m[1])]=min(5000,hist.get((m[0],m[1]),0)+d*d)
            break
    fl=1 if best<=alpha else 2 if best>=b else 0
    if len(tt)<50000:tt[tk]=(d,best,fl,bm)
    return best

def choose(g):
    global nodes,tt,hist,killers,reps
    setup(g.get('start_fen') or g['fen']);nodes=0;tt={};hist={};killers=[[] for _ in range(80)];reps={}
    if g.get('start_fen'):
        reps[key()]=1
        for x in g.get('moves',[]):
            m=parse(x);o=push(m);reps[key()]=reps.get(key(),0)+1
    else:reps[key()]=1
    ms=[parse(x) for x in g.get('legal_moves',[])]
    if not ms:return ''
    best=ms[0];ms.sort(key=lambda m:order(m,0,None),reverse=True)
    for depth in range(1,40):
        alpha=-MATE;pick=best;score=-MATE
        try:
            for i,m in enumerate(ms):
                o=push(m);k=key();reps[k]=reps.get(k,0)+1
                try:
                    if i==0:v=-search(depth-1,-MATE,-alpha,1)
                    else:
                        v=-search(depth-1,-alpha-1,-alpha,1)
                        if v>alpha:v=-search(depth-1,-MATE,-alpha,1)
                finally:
                    reps[k]-=1
                    if not reps[k]:del reps[k]
                    pop(m,o)
                if v>score:score=v;pick=m
                if v>alpha:alpha=v
        except Stop:break
        best=pick;ms.remove(best);ms.insert(0,best)
    return uci(best)

def run():
    while True:
        g=post('/game',{})
        if g.get('status')=='done':break
        while g.get('status')=='your_move':
            m=choose(g)
            g=post('/resign' if not m else '/move',{'game':g['game']} if not m else {'game':g['game'],'move':m})
        print('game %s: %s by %s'%(g.get('game'),g.get('outcome'),g.get('reason')),file=sys.stderr)
    print('done: %s won, %s drawn, %s lost'%(g.get('wins'),g.get('draws'),g.get('losses')),file=sys.stderr)

if __name__=='__main__':run()
