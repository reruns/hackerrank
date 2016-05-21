#abstracts comparison logic for counting
def comp(x,xm,c,d)
    if x == xm
        return [x, c+1]
    elsif d
        if x > xm
            return [x,1]
        end
    else
        if x < xm
            return [x, 1]
        end
    end
    [xm,c]
end

#compute the binomial coefficient
def binc(n,k)
    def f(n_,k_,a,b)
        return (a / b) if k_ == 0
        return f(n_-1, k_-1, a * n_, b * k_)
    end
    f(n,k,1,1)
end

n,k = gets.chomp.split.map(&:to_i)
minx = 1.0 / 0 #INF
miny = 1.0 / 0
maxx = 0
maxy = 0

minxc = 0
minyc = 0
maxxc = 0
maxyc = 0

#count how many stains are on each edge of the initial rectangle
n.times do
    x,y = gets.chomp.split.map(&:to_i)
    maxx, maxxc = comp(x, maxx, maxxc, true)
    minx, minxc = comp(x, minx, minxc, false)
    maxy, maxyc = comp(y, maxy, maxyc, true)
    miny, minyc = comp(y, miny, minyc, false)
end

sum = 0
[maxxc,minxc,maxyc,minyc].each do |num|
    if num <= k
       sum += binc(n-num, k-num)
    end
end

[[maxxc,minxc],[maxxc,maxyc],[maxxc,minyc],[minxc,maxyc],[minxc,minyc],[maxyc,minyc]].each do |pair|
    if pair[0] + pair[1] <= k
        sum -= binc(n-pair[0]-pair[1],k-pair[0]-pair[1])
    end
end

p sum

