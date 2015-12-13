def probe(grid, pos)
    region = 0
    positions = [pos]
    while(!positions.empty?)
        x,y = positions.pop
        next if (x < 0 || y < 0 || x >= grid.length || y >= grid[0].length)
        if(grid[x][y] == 1)
            region += 1
            grid[x][y] = 0
            positions.push([x,y+1],[x,y-1],[x+1,y+1],[x+1,y],[x+1,y-1],[x-1,y+1],[x-1,y],[x-1,y-1])
        end
    end
    region
end

n = gets.strip.to_i
m = gets.strip.to_i

grid = []
for i in (0..n-1)
    grid[i] = gets.strip.split.map(&:to_i)
end

region_max = 0

for i in (0..n-1)
    for j in (0..m-1)
        region_max = [region_max, probe(grid,[i,j])].max if grid[i][j] == 1
    end
end

print region_max