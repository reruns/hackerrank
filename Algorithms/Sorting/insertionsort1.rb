def insertionSort( ar )
  n = ar.last
  ar.reverse.each_with_index do |x,i|
    next if i == 0
    if x > n
      ar[-i] = x
      puts nprint(ar)
      if i == ar.length - 1
        ar[0] = n
        puts nprint(ar)
      end
    else
      ar[-i] = n
      puts nprint(ar)
      break
    end
  end
end

def nprint(ar)
  out = ""
  ar.each do |x|
    out << x.to_s << ' '
  end
  out
end

count = gets.to_i
ar = gets.strip.split.map { |i| i.to_i }

insertionSort( ar )

10
2 3 4 5 6 7 8 9 10 1
