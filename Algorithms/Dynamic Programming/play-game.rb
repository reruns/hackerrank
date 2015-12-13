f = File.new("testinput.txt")
t = f.gets.chomp.to_i

t.times do
    n = f.gets
    a = f.gets.chomp.split.map(&:to_i)
    game = Hash.new do |h,k|
        if (a.length - k - 1) < 3
            h[k] = [a[k..a.length].inject(:+),0]
        else
            h[k] = [1,2,3].map do |n|
                pred = h[k+n].rotate
                [pred[0] + a[k..k+n-1].inject(:+), pred[1]]
            end.max {|a,b| a[0] <=> b[0]}
        end
    end

    (1..a.length).to_a.each do |x|
        y = game[a.length - x]
    end
    puts game[0]
end