#AVL Tree implementation with a bunch of features we don't need stripped out.
class AVLNode
    attr_reader :key, :value, :height, :left, :right
    attr_writer :left, :right
    
    def initialize(key, value)
        @key, @value = key, value
        @left = @right = EMPTY
        @height = 1
    end
    
    def empty?
        false
    end
    
    def size
        @left.size + @right.size + 1
    end
    
    def insert(key, value)
        case key <=> @key
        when -1
            @left = @left.insert(key, value)
        when 0
            @value = value
        when 1
            @right = @right.insert(key, value)
        end
        rotate
    end
    
    def update_height
        @height = (@left.height > @right.height ? @left.height : @right.height) + 1
    end
    
    def rotate
        case @left.height - @right.height
        when 2
            if @left.left.height < @left.right.height
                @left = @left.rotate_left
            end
            root = rotate_right
        when -2
            if @right.left.height > @right.right.height
                @right = @right.rotate_right
            end
            root = rotate_left
        else
            root = self
        end
        root.update_height
        root
    end
    
    
    def rotate_left
        root = @right
        @right = root.left
        root.left = self
        root.left.update_height
        root
    end
    
    def rotate_right
        root = @left
        @left = root.right
        root.right = self
        root.right.update_height
        root
    end
    
    #searches for the smallest element in the tree greater than tar
    #nb: higher is the name of a Java method that does the same thing with a set
    def higher(tar)
        cur = self
        parent = 0
        while(true)
            if cur.key == tar
                if cur.right.key.nil? && parent < tar
                    return 0
                elsif cur.right.key.nil?
                    return parent
                else
                    return cur.leftmostright.key
                end
            elsif cur.key < tar
                cur = cur.right
            else
                parent = cur.key
                cur = cur.left                
            end
        end
    end
    
    def leftmostright
        cur = self.right
        while (!cur.left.key.nil?)
            cur = cur.left
        end
        cur
    end
    
    class EmptyNode < AVLNode
        def initialize
            @key = nil
            @value = nil
            @height = 0
        end
        
        def empty?
            true
        end
        
        def size
            0
        end
        
        def insert(key, value)
            AVLNode.new(key, value)
        end
        
        def rotate
            self
        end
    end

EMPTY = AVLNode::EmptyNode.new.freeze
end

f = File.new("testinput.txt")
o = File.new("testoutput.txt", "r+")

t = f.gets.chomp.to_i

t.times do
    x,m = f.gets.chomp.split.map(&:to_i)
    n = f.gets.chomp.split.map(&:to_i)
    
    a = AVLNode.new(0,0)
    k = 0
    max = 0
    
    n.each do |el|
        k = (k + el) % m
        a = a.insert(k,k)
        diff = (k - a.higher(k)) % m
        max = [max,diff].max
    end
    
    o.puts max
end
