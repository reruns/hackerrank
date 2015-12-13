t = gets.chomp.to_i

t.times do
    n,k = gets.chomp.split.map(&:to_i)
    a = gets.chomp.split.map(&:to_i).uniq
    a.reject!{ |x| a.any?{|y| x % y == 0 && x != y }}
    rem = Hash.new do |h,k|
        if k == 0 || a.include?(1)
            h[k] = 0
        elsif k < a.min
            h[k] = k
        else
            h[k] = a.select{ |x| x <= k}.map{|x| h[k-x]}.min
        end
    end
    puts k-rem[k]
end