#This is not dynamic programming...
t = gets.chomp.to_i

t.times do
    n = gets.chomp.to_i
    nums = gets.chomp.split.map(&:to_i)
    cur_max = 0
    total = 0
    
    nums.reverse.each do |price|
        if price > cur_max
            cur_max = price
        else
            total += cur_max - price
        end
    end
    puts total
end