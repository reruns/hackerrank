#!/bin/ruby

t = gets.strip.to_i
for a0 in (0..t-1)
    R,C = gets.strip.split(' ').map(&:to_i)
    G = Array.new(R)
    
    #get the grid
    for G_i in (0..R-1)
        G[G_i] = gets.strip.split('').map(&:to_i)
    end
    r,c = gets.strip.split(' ').map(&:to_i)
    P = Array.new(r)
    
    #get the target
    for P_i in (0..r-1)
        P[P_i] = gets.strip.split('').map(&:to_i)
    end
    
    puts findPattern(G,P)
end

def findPattern(grid, pattern)
    for i in (0..grid.length-pattern.length-1)
        for j in (0.. grid[0].length-pattern[0].length-1)
            
        end
    end
end