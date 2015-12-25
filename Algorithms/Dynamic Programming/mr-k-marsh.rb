class Marsh
    attr_reader :m, :n, :left, :up
    def initialize
        @m,@n = gets.chomp.split.map(&:to_i)
    
        @up = Array.new(@m) {Array.new}
        @left = Array.new(@m) {Array.new}
        (0..@m-1).each do |i|
            gets.chomp.split('').each_with_index do |c, j|
                if c == 'x'
                    @up[i][j] = @left[i][j] = -1
                else
                    if i == 0
                        @up[i][j] = 0
                    else @up[i][j] = @up[i-1][j] +1
                    end
                    if j == 0
                        @left[i][j] = 0
                    else @left[i][j] = @left[i][j-1]+1
                    end
                end
            end
        end
    end
    
    def maxperim()
        q = [[0,0,@m-1,@n-1]]
        m = 0
        while(!q.empty?)
            k = q.pop
            if @perim.has_key?(k)
                m = @perim[k] > m ? @perim[k] : m
            else
                @perim[k] = 0
                yi, xi, yf, xf = k
                p = (yf-yi)*2 + (xf-xi)*2
                next if xi >= xf || yi >= yf || p <= m
                
                if @left[yi][xf] < xf - xi
                    l = @left[yi][xf]
                    q.push([yi,xf-l,yf,xf],[yi,xi,yf,xf-l-2],[yi+1,xi,yf,xf])
                elsif @up[yf][xi] < yf - yi
                    u = @up[yf][xi]
                    q.push([yf-u,xi,yf,xf], [yi,xi,yf-u-2,xf],[yi,xi+1,yf,xf])
                elsif @up[yf][xf] < yf - yi
                    u = @up[yf][xf]
                    q.push([yf-u,xi,yf,xf], [yi,xi,yf-u-2,xf],[yi,xi,yf,xf-1])
                elsif @left[yf][xf] < xf - xi
                    l = @left[yf][xf]
                    q.push([yi,xf-l,yf,xf], [yi,xi,yf,xf-l-2], [yi,xi,yf-1,xf])
                else #we're done!
                    @perim[k] = p
                    m = p > m ? p : m
                end 
            end
        end
        @perim[[0,0,@m-1,@n-1]] = m
        m
    end
end

a = Marsh.new()

puts a.maxperim