t = gets.chomp.to_i

t.times do
    n,m = gets.chomp.split.map(&:to_i)
    grid = []
    n.times do
        grid.push(gets.chomp.split(''))
    end
    k = gets.chomp.to_i
    start = []
    
    for i in (0 to n-1)
        for j in (0 to m-1)
            if grid[i][j] == 'M'              
                start = [i,j,0]
                break 
            end
        end
    end
    
    queue = [start]
    finished = false
    while(!finished)
        i,j,c = queue.pop
        if grid[i][j] == "*"
            finished = true
            if (k == c)
                puts "Impressed"
            else puts "Oops!"
            end
        end
        
        paths = []
        [[1,0],[0,1],[-1,0],[0,-1]].each do |del|
            q,p = del
            next if i+q < 0 || i+q >= n ||  j+p < 0 || j+p >= m
            paths << [i+q,j+p] if grid[i+q][j+p] != "X"
        end
        paths.each do |path|
            path << (paths.length > 1 ? c+1 : c)
            queue.push(path)
        end
        
        grid[i][j] = "X"
    end
end