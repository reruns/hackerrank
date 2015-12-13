n = $stdin.gets.to_i
a = $stdin.gets.chomp.split(' ').map(&:to_i)
q = $stdin.gets.to_i

q.times do 
    ns = $stdin.gets.chomp.split(' ').map(&:to_i)
    x = ns[0]
    y = ns[1]
    if x != y && a[x] == 0
    then puts "Odd"
    elsif a[x-1].odd?
    then puts "Odd"
    else puts "Even"
    end
end