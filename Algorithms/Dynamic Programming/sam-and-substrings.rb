n = gets.chomp

candy = Hash.new do |h,k|
    v = k.to_i
    if v <= 9
        h[k] = v
    else
        h[k] = v + h[(v / 10).to_s] + h[k[1..v.length]]
    end
end

print candy[n]