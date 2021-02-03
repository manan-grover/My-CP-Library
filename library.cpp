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
#define PBS(x) tree<x,null_type,less<I>,rb_tree_tag,tree_order_statistics_node_update>
#define PBM(x,y) tree<x,y,less<I>,rb_tree_tag,tree_order_statistics_node_update>

//................DSU.....................

class DSU{
private:
  V(I) parent;
  V(I) size;
public:
  DSU(I n){
    asc(i,0,n+1){
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
  asc(i,1,n+1){
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
  if(a.fi*(b.se-c.se)+b.fi*(c.se-a.se)+c.fi*(a.se-b.se)<0){
    return 1;
  }else if(a.fi*(b.se-c.se)+b.fi*(c.se-a.se)+c.fi*(a.se-b.se)>0){
    return -1;
  }else{
    return 0;
  }
}
void chull(V(P(I,I)) a,V(P(I,I)) &b,B f) {
  if (a.size()==1){
    b=a;
    return;
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




//.......................Main........................
int main(){
  ios_base::sync_with_stdio(false);cin.tie(NULL);cout.tie(NULL);
  #ifndef ONLINE_JUDGE
  freopen("input.txt", "r", stdin);
  freopen("output.txt", "w", stdout);
  #endif
  
  return 0;
}