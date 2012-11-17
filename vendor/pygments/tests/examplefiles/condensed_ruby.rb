# Server: ruby p2p.rb password server server-uri merge-servers
# Sample: ruby p2p.rb foobar server druby://localhost:1337 druby://foo.bar:1337
# Client: ruby p2p.rb password client server-uri download-pattern
# Sample: ruby p2p.rb foobar client druby://localhost:1337 *.rb
require'drb';F,D,C,P,M,U,*O=File,Class,Dir,*ARGV;def s(p)F.split(p[/[^|].*/])[-1
]end;def c(u);DRbObject.new((),u)end;def x(u)[P,u].hash;end;M=="client"&&c(U).f(
x(U)).each{|n|p,c=x(n),c(n);(c.f(p,O[0],0).map{|f|s f}-D["*"]).each{|f|F.open(f,
"w"){|o|o<<c.f(p,f,1)}}}||(DRb.start_service U,C.new{def f(c,a=[],t=2)c==x(U)&&(
t==0&&D[s(a)]||t==1&&F.read(s(a))||p(a))end;def y()(p(U)+p).each{|u|c(u).f(x(u),
p(U))rescue()};self;end;private;def p(x=[]);O.push(*x).uniq!;O;end}.new.y;sleep)
