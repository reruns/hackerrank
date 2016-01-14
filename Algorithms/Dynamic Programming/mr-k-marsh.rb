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
    
    def perimSearch()
        best = 0
        (0..@m-2).each do |py|
            (0..@n-2).each do |j|
                px = @n - j - 1
                l = left[py][px]
                if l >= 1 #We have a valid top row
                    (py+1..@m-1).each do |ty| #Search potential bottom rows
                        break if up[ty][px] < ty-py
                        if up[ty][px-l] < ty-py
                            while up[ty][px-l] < ty-py && l > 0
                                l -= 1
                            end
                            next
                        end
                        break if l == 0
                        perim = 2*l + 2*(ty-py)
                        next if perim <= best #Too small to matter.
                        next if left[ty][px] < l #There's an X on this row somewhere
                        best = perim
                    end
                end
            end
        end
        best
    end
end

a = Marsh.new()

puts a.maxperim