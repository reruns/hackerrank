#!/bin/ruby

n = gets.strip.to_i
grid = Array.new(n)
for grid_i in (0..n-1)
    grid[grid_i] = gets.chomp.split('').map(&:to_i)
end

for i in (1..grid.length-2)
    for j in (1..grid[i].length-2)
        x = grid[i][j]
        if [[1,1],[1,-1],[-1,1],[-1,-1]].all? {|a| x < grid[i+a[0]][j+a[1]]}
            print "X"
        else print x
        end
    end
    puts
end