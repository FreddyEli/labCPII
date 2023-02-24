using OffsetArrays

#hbar= 1.054571817*10^(-34)
#m=9.1093837015*10^(-31)
hbar=1.
m=1.
well_a=1.
n_well=1.

#functions
function p(r)::Float64
	return   zero(x)
       end
function q(r)::Float64
	return -(n_well*n_well*3.1416*3.1416)/(well_a*well_a)
       end
#function E(r)::Float64
function r(r)::Float64
	return   zero(x)
end


#initial conditions
n=10
x0=0.
xf=well_a
fx0=10.
fxf=10.
h=(xf-x0)/(n+1)
a=zeros(n)
b=zeros(n)
c=zeros(n)
d=zeros(n)
l=zeros(n)
u=zeros(n)
z=zeros(n)
w=OffsetArray{Float64}(undef, 0:n+1)
w[0]=fx0
w[n+1]=fxf
#display(w)

#main
##for first inner point
x=x0+h
a[1]=2+h*h*q(x) 
b[1]=-1+h*p(x)/2 
d[1]=-h*h*r(x)+(1+h*p(x)/2)*fx0 

##for inner inner point
for i = 2:n-1
	global x=x0+h*i
	a[i]=2+h*h*q(x) 
	b[i]=-1+h*p(x)/2 
	c[i]=-1-h*p(x)/2 
	d[i]=-h*h*r(x)
end

##for last inner point
x=xf-h
a[n]=2+h*h*q(x) 
c[n]=-1-h*p(x)/2 
d[n]=-h*h*r(x)+(1-h*p(x)/2)*fxf 

##vectors a,b,c,d are populated
#display(a)
#display(b)
#display(c)

#Solving the matrix
l[1]=a[1]
u[1]=b[1]/a[1]
z[1]=d[1]/l[1]


for i = 2:n-1
	l[i]=a[i]-c[i]*u[i-1]
	u[i]=b[i]/l[i]
	z[i]=(d[i]-c[i]*z[i-1])/l[i]
end

l[n]=a[n]-c[n]*u[n-1]
z[n]=(d[n]-c[n]*z[n-1])/l[n]

for i = 1:n
	w[i]=z[i]
end
#display(w)

for i = n-1:-1:1
	w[i]=z[i]-u[i]*w[i+1]
end
#display(w)

#Printing
io = open("sol", "w")
	x=x0
	for i = 0:n+1
	println(io, x, "  ",  w[i])
		global x=x+h
	end
close(io)


