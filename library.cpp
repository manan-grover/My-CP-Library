//..............Author: mexomerf...............

#include <bits/stdc++.h>
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/tree_policy.hpp>
using namespace std;
using namespace __gnu_pbds;
#define asc(i,a,n) for(I i=a;i<n;i++)
#define dsc(i,a,n) for(I i=n-1;i>=a;i--)
#define forw(it,x) for(A it=(x).begin();it!=(x).end();it++)
#define bacw(it,x) for(A it=(x).rbegin();it!=(x).rend();it++)
#define pb push_back
#define mp make_pair
#define fi first
#define se second
#define lb(x) lower_bound(x)
#define ub(x) upper_bound(x)
#define fbo(x) find_by_order(x)
#define ook(x) order_of_key(x)
#define all(x) (x).begin(),(x).end()
#define sz(x) (I)((x).size())
#define clr(x) (x).clear()
#define U unsigned
#define I long long int
#define S string
#define C char
#define D long double
#define A auto
#define B bool
#define CM(x) complex<x>
#define V(x) vector<x>
#define P(x,y) pair<x,y>
#define OS(x) set<x>
#define US(x) unordered_set<x>
#define OMS(x) multiset<x>
#define UMS(x) unordered_multiset<x>
#define OM(x,y) map<x,y>
#define UM(x,y) unordered_map<x,y>
#define OMM(x,y) multimap<x,y>
#define UMM(x,y) unordered_multimap<x,y>
#define L(x) list<x>
#define Q(x) queue<x>
#define PBS(x) tree<x,null_type,less<I>,rb_tree_tag,tree_order_statistics_node_update>
#define PBM(x,y) tree<x,y,less<I>,rb_tree_tag,tree_order_statistics_node_update>
#define pi (D)acos(-1)
#define md 1000000007

//................DSU.....................

class DSU{
private:
  V(I) parent;
  V(I) size;
public:
  DSU(I n){
    asc(i,0,n){
      parent.pb(i);
      size.pb(1);
    }
  }
  I find(I a){
    if(parent[a]==a){
      return a;
    }
    I temp=find(parent[a]);
    parent[a]=temp;
    return temp;
  }
  void merge(I a,I b){
    a=find(a);
    b=find(b);
    if(a==b){
      return;
    }
    if(size[a]<size[b]){
      swap(a,b);
    }
    size[a]+=size[b];
    parent[b]=a;
  }
};

//...............Mod...................

I modex(I a,I b,I m){
  a=a%m;
  if(b==0){
    return 1;
  }
  I temp=modex(a,b/2,m);
  temp=(temp*temp)%m;
  if(b%2){
    temp=(temp*a)%m;
  }
  return temp;
}
I mod(I a,I b,I m){
  a=a%m;
  b=b%m;
  I c=__gcd(a,b);
  a=a/c;
  b=b/c;
  c=modex(b,m-2,m);
  return (a*c)%m;
}

//...............Combination.....................

I ncr(I n,I r,I fact[],I m){
  if(n<0 || r<0){
    return 0;
  }
  if(n<r){
    return 0;
  }
  return mod(fact[n],fact[n-r]*fact[r],m);
}

//.............Segment Tree..................

class seg{
public:
  struct node{
    //........start
    I sum;
    //........end
    I lft,rgt;
  };
  I m;
  V(node) segarr;
  node merge(node a,node b){
    node ans;
    ans.lft=min(a.lft,b.lft);
    ans.rgt=max(a.rgt,b.rgt);
    //..............start
    ans.sum=a.sum+b.sum;
    //..............end
    return ans;
  }
  void make(node &temp,I a,B f){
    if(f){
      //.......start
      temp.sum=a;
      //.......end
    }else{
      //.......start
      temp.sum=0;
      //.......end
    }
  }
  seg(I n,I arr[]){
    m=pow(2,ceil(log2(n)));
    node temp;
    asc(i,0,2*m-1){
      segarr.pb(temp);
    }
    asc(i,0,m){
      if(i<n){
        make(segarr[i+m-1],arr[i],true);
      }else{
        make(segarr[i+m-1],arr[i],false);
      }
      segarr[i+m-1].lft=i;
      segarr[i+m-1].rgt=i;
    }
    dsc(i,0,m-1){
      segarr[i]=merge(segarr[2*i+1],segarr[2*i+2]);
    }
  }
  node query_help(I l,I r,I x){
    if(segarr[x].lft>=l && segarr[x].rgt<=r){
      return segarr[x];
    }
    if(l>segarr[2*x+1].rgt){
      return query_help(l,r,2*x+2);
    }
    if(r<segarr[2*x+2].lft){
      return query_help(l,r,2*x+1);
    }
    return merge(query_help(l,r,2*x+1),query_help(l,r,2*x+2));
  }
  node query(I l,I r){
    return query_help(l,r,0);
  }
  void update_help(I x){
    segarr[x]=merge(segarr[2*x+1],segarr[2*x+2]);
    if(x!=0){
      update_help((x-1)/2);
    }
  }
  void update(I x,I temp){
    I y=x+m-1;
    make(segarr[y],temp,true);
    if(y!=0){
      update_help((y-1)/2);
    }
  }
};

//.............Minimum Spanning Tree.............

I mntree(V(P(I,I)) gr[],V(P(I,I)) tr[],I n){
  DSU d(n);
  OS(P(I,P(I,I))) s;
  asc(i,0,n){
    asc(j,0,sz(gr[i])){
      s.insert({gr[i][j].se,{i,gr[i][j].fi}});
    }
  }
  I tot=0;
  I cnt=0;
  forw(it,s){
    if(cnt==n){
      break;
    }
    I a=(*it).se.fi;
    I b=(*it).se.se;
    I w=(*it).fi;
    if(d.find(a)!=d.find(b)){
      d.merge(a,b);
      tot+=w;
      tr[a].pb({b,w});
      tr[b].pb({a,w});
      cnt++;
    }
  }
  return tot;
}

//................Convex Hull....................

I pos(P(I,I) a, P(I,I) b, P(I,I) c){
  I k=a.fi*(b.se-c.se)+b.fi*(c.se-a.se)+c.fi*(a.se-b.se);
  if(k<0){
    return 1;
  }else if(k>0){
    return -1;
  }else{
    return 0;
  }
}
V(P(I,I)) chull(V(P(I,I)) a,B f){
  //if f then include extra
  V(P(I,I)) b;
  if(a.size()==1){
    return a;
  }
  I xx=0;
  if(f){
    xx++;
  }
  sort(all(a));
  P(I,I) p1=a[0],p2=a.back();
  V(P(I,I)) u,d;
  u.pb(p1);
  d.pb(p1);
  asc(i,1,sz(a)-1){
    if(pos(p1,a[i],p2)>-xx){
      while(sz(u)>=2 && pos(u[sz(u)-2],u[sz(u)-1],a[i])<1-xx){
        u.pop_back();
      }
      u.pb(a[i]);
    }else if(pos(p1,a[i],p2)<xx){
      while(sz(d)>=2 && pos(d[sz(d)-2],d[sz(d)-1],a[i])>xx-1){
        d.pop_back();
      }
      d.pb(a[i]);
    }
  }
  while(sz(u)>=2 && pos(u[sz(u)-2],u[sz(u)-1],p2)<1-xx){
    u.pop_back();
  }
  while(sz(d)>=2 && pos(d[sz(d)-2],d[sz(d)-1],p2)>xx-1){
    d.pop_back();
  }
  u.pb(p2);
  asc(i,0,sz(u)){
    b.pb(u[i]);
  }
  dsc(i,1,sz(d)){
    b.pb(d[i]);
  }
  return b;
}

//......................2*area...................

D areaX2(V(P(I,I)) v){
  if(sz(v)<3){
    return 0;
  }
  I ans=0;
  asc(i,1,sz(v)-1){
    I temp=abs(v[0].fi*v[i].se+v[i].fi*v[i+1].se+v[i+1].fi*v[0].se-v[0].se*v[i].fi-v[i].se*v[i+1].fi-v[i+1].se*v[0].fi);
    if(pos(v[0],v[i],v[i+1])==1){
      ans+=temp;
    }else{
      ans-=temp;
    }
  }
  ans=abs(ans);
  return ans;
}

//.................distance squared.................

I dis2(P(I,I) a,P(I,I) b){
  I ans=(a.fi-b.fi)*(a.fi-b.fi)+(a.se-b.se)*(a.se-b.se);
  return ans;
}

//....................Binary Search................

B check(I x){
  return true;
}
I bins(I l,I r){
  if(r-l<2){
    if(check(r)){
      return r;
    }else{
      return l;
    }
  }
  I mid=(r+l)/2;
  if(check(mid)){
    return bins(mid,r);
  }else{
    return bins(l,mid-1);
  }
}

//...................Ternary Search....................

I func(I x){
  return 0;
}
I trins(I l,I r){
  if(r-l<3){
    I val=func(l);
    I ans=l;
    asc(i,l+1,r+1){
      I x=func(i);
      if(val<x){
        val=x;
        ans=i;
      }
    }
    return ans;
  }
  I m1=(2*l+r)/3;
  I m2=(l+2*r)/3;
  I f1=func(m1);
  I f2=func(m2);
  if(f1<f2){
    return trins(m1+1,m2);
  }else if(f1>f2){
    return trins(m1,m2-1);
  }else{
    return trins(m1+1,m2-1);
  }
}

//.....................Dijkstra's Algorithm...............

void djik(I x,I n,I dis[],V(P(I,I)) gr[]){
  OMM(I,I) mpp;
  I vis[n]={};
  mpp.insert(mp(0,x));
  while(sz(mpp)){
    A it=mpp.begin();
    I w=(*it).fi;
    I y=(*it).se;
    mpp.erase(it);
    if(vis[y]==0){
      vis[y]=1;
      dis[y]=w;
      asc(i,0,sz(gr[y])){
        mpp.insert(mp(w+gr[y][i].se,gr[y][i].fi));
      }
    }
  }
}

//......................Prime Sieve..................

void sieve(I n,I pf[],V(I) &prm,I r[]){
  asc(i,0,n){
    pf[i]=0;
  }
  //....start...
  asc(i,0,n){
    r[i]=i;
  }
  //....end.....
  asc(i,2,n){
    if(pf[i]==0){
      prm.pb(i);
      I x=i;
      I k=i*i;
      while(x<n){
        pf[x]++;
        //....start....
        while(r[x]%k==0){
          r[x]/=k;
        }
        //....end......
        x+=i;
      }
    }
  }
}

//.......................KMP.........................

void lps(S &s,I p[]) {
  p[0]=0;
  asc(i,1,sz(s)){
    I j=p[i-1];
    while(j>0 && s[i]!=s[j]){
      j=p[j-1];
    }
    if(s[i]==s[j]){
      j++;
    }
    p[i]=j;
  }
}

//.....................Z Function....................

void zfn(S &s,I z[]) {
  asc(i,0,sz(s)){
    z[i]=0;
  }
  I l=0;
  I r=0;
  z[0]=sz(s);
  asc(i,1,sz(s)){
    if(i<=r){
      z[i]=min(r-i+1,z[i-l]);
    }
    while(i+z[i]<sz(s) && s[z[i]]==s[i+z[i]]){
      z[i]++;
    }
    if(i+z[i]-1>r){
      l=i;
      r=i+z[i]-1;
    }
  }
}

//....................Round.........................

D round(D x,I l){
  l=pow(10,l);
  x*=l;
  round(x);
  x/=l;
  return x;
}

//.................Mos Algorithm......................

I bl;
B moscomp(P(P(I,I),I) a,P(P(I,I),I) b){
  if(a.fi.fi/bl == b.fi.fi/bl){
    return a.fi.se<b.fi.se;
  }else{
    return a.fi.fi/bl<b.fi.fi/bl;
  }
}
void mos(P(P(I,I),I) qr[],I q,I n,I ans[],I a[]){
  bl=ceil(sqrt(n));
  sort(qr,qr+q,moscomp);
  I l=-1;
  I r=-1;
  asc(i,0,q){
    while(qr[i].fi.fi<l){
      //.....start......
      //add a[l]
      //......end.......
      l--;
    }
    while(qr[i].fi.se>r){
      r++;
      //.....start......
      //add a[r]
      //......end.......
    }
    while(qr[i].fi.fi>l){
      l++;
      //.....start......
      //rem a[l]
      //......end.......
    }
    while(qr[i].fi.se<r){
      //.....start......
      //rem a[r]
      //......end.......
      r--;
    }
    //.....start.........
    ans[qr[i].se]=0;
    //......end..........
  }
}

//..................Dinic Maximum Flow................

class dinic{
public:
  struct FlowEdge{
    I v,u;
    I cap;
    I flow=0;
    //v--cap-->u
  };
  OM(P(I,I),I) edge_adr;
  V(FlowEdge) edges;
  V(V(I)) adj;
  I n;
  I m=0;
  I s,t;
  V(I) level,ptr;
  Q(I) q;
  dinic(I n,I s,I t){
    this->n=n;
    this->s=s;
    this->t=t;
    adj.resize(n);
    level.resize(n);
    ptr.resize(n);
  }
  void add_edge(I v,I u,I cap){
    FlowEdge temp;
    temp.v=v;
    temp.u=u;
    temp.cap=cap;
    edges.pb(temp);
    temp.v=u;
    temp.u=v;
    temp.cap=0;
    edges.pb(temp);
    edge_adr.insert(mp(mp(v,u),m));
    edge_adr.insert(mp(mp(u,v),m+1));
    adj[v].pb(m);
    adj[u].pb(m+1);
    m+=2;
  }
  void uni_edge(I v,I u,I cap){
    A it=edge_adr.find(mp(v,u));
    if(it==edge_adr.end()){
      add_edge(v,u,cap);
    }else{
      edges[(*it).se].cap=cap;
    }
  }
  B bfs(){
    while(!q.empty()){
      int v = q.front();
      q.pop();
      I id;
      asc(i,0,sz(adj[v])){
        id=adj[v][i];
        if(edges[id].cap-edges[id].flow<1){
          continue;
        }
        if(level[edges[id].u]!=-1){
          continue;
        }
        level[edges[id].u]=level[v]+1;
        q.push(edges[id].u);
      }
    }
    return level[t]!=-1;
  }
  I dfs(I v,I pushed){
    if(pushed==0){
      return 0;
    }
    if(v==t){
      return pushed;
    }
    while(ptr[v]<sz(adj[v])){
      I id=adj[v][ptr[v]];
      I u=edges[id].u;
      if(level[v]+1!=level[u] || edges[id].cap-edges[id].flow<1){
        ptr[v]++;
        continue;
      }
      I tr=dfs(u,min(pushed,edges[id].cap-edges[id].flow));
      if(tr==0){
        ptr[v]++;
        continue;
      }
      edges[id].flow+=tr;
      edges[id^1].flow-=tr;
      return tr;
      ptr[v]++;
    }
    return 0;
  }
  I flow(){
    I f=0;
    while(1){
      fill(all(level),-1);
      level[s]=0;
      q.push(s);
      if(!bfs()){
        break;
      }
      fill(all(ptr),0);
      I pushed;
      while(1){
        pushed=dfs(s,LLONG_MAX);
        if(pushed){
          f+=pushed;
        }else{
          break;
        }
      }
    }
    asc(i,0,sz(edges)){
      edges[i].flow=0;
    }
    return f;
  }
};

//......Lowest Common Ancistor(LCA) & Binary Lifting.......

class binl{
public:
  V(I) lvl;
  V(V(I)) anc;
  void dfs(I x,I pr,V(I) tr[]){
    if(pr==-1){
      lvl[x]=0;
    }else{
      lvl[x]=lvl[pr]+1;
      anc[x].pb(pr);
      I curr=0;
      while(curr<sz(anc[anc[x].back()])){
        anc[x].pb(anc[anc[x].back()][curr]);
        curr++;
      }
    }
    asc(i,0,sz(tr[x])){
      if(tr[x][i]!=pr){
        dfs(tr[x][i],x,tr);
      }
    }
  }
  binl(I n,V(I) tr[],I x){
    anc.resize(n);
    lvl.resize(n);
    dfs(x,-1,tr);
  }
  I lca(I u,I v){
    if(lvl[u]<lvl[v]){
      swap(u,v);
    }
    I cnt=0;
    I m=lvl[u]-lvl[v];
    while(m){
      if(m%2){
        u=anc[u][cnt];
      }
      cnt++;
      m/=2;
    }
    if(u==v){
      return u;
    }
    I k=sz(anc[u]);
    dsc(i,0,k){
      if(i<sz(anc[u])){
        if(anc[u][i]!=anc[v][i]){
          u=anc[u][i];
          v=anc[v][i];
        }
      }
    }
    return anc[u][0];
  }
  I dis(I u,I v){
    return lvl[u]+lvl[v]-2*lvl[lca(u,v)];
  }
};

//.......................Main........................

int main(){
  srand(time(NULL));
  ios_base::sync_with_stdio(false);cin.tie(NULL);cout.tie(NULL);
  #ifndef ONLINE_JUDGE
  freopen("input.txt", "r", stdin);
  freopen("output.txt", "w", stdout);
  #endif
  
  return 0;
}