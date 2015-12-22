#Ruby's default max stack depth is too low for the test cases.
#Otherwise, this solution should be fast enough.
f = File.new("testinput.txt")
n = f.gets.chomp.to_i
weights = f.gets.chomp.split.map(&:to_i)
tree = Array.new(n) {Array.new}

(n-1).times do
    a,b = f.gets.chomp.split.map(&:to_i)
    tree[a-1].push(b-1)
    tree[b-1].push(a-1)
end

totalWeights = Hash.new do |h,k|
    h[k[0]] = weights[k[0]] + (tree[k[0]] - [k[1]]).map { |l| h[[l,k[0]]] }.inject(0, :+)
    h[k] = h[k[0]]
end

#We can choose any node as our root, and it should work.
totalWeights[0]

min = (1..n-1).to_a.inject(totalWeights[0]) do |mem, node|
    val = (2 * totalWeights[node] - totalWeights[0]).abs
    val < mem ? val : mem
end
puts min
