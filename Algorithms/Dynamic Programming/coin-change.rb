n = gets.split[0].to_i
coins = gets.split.map(&:to_i).sort{|a,b| b <=> a}

change_count = Hash.new do |h,k|
    amt = k[0]
    cs = k[1]
    if amt == 0
        h[k] = 1
    elsif cs.empty? || amt < cs.min
        h[k] = 0
    else
        h[k] = h[n, cs.drop(1)] + h[amt - cs[0], cs]
    end
end
change_count[[n,coins]]