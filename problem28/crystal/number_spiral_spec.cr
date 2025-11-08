require "spec"
require "./number_spiral"

describe NumberSpiral do
  describe "#initialize" do
    it "creates spiral with odd size" do
      spiral = NumberSpiral.new(5)
      spiral.should_not be_nil
    end

    it "raises error for even size" do
      expect_raises(ArgumentError, "Size must be odd") do
        NumberSpiral.new(4)
      end
    end

    it "accepts size 1" do
      spiral = NumberSpiral.new(1)
      spiral.should_not be_nil
    end
  end

  describe "#diagonal_sum" do
    it "returns 1 for 1×1 spiral" do
      NumberSpiral.new(1).diagonal_sum.should eq(1)
    end

    it "returns 25 for 3×3 spiral" do
      # 3×3 spiral:
      # 7 8 9
      # 6 1 2
      # 5 4 3
      # Diagonals: 7+5+3+9+1 = 25
      NumberSpiral.new(3).diagonal_sum.should eq(25)
    end

    it "returns 101 for 5×5 spiral" do
      # 5×5 spiral:
      # 21 22 23 24 25
      # 20  7  8  9 10
      # 19  6  1  2 11
      # 18  5  4  3 12
      # 17 16 15 14 13
      # Diagonals: 21+7+1+3+13 + 25+9+2+5+17 - 1 = 101
      NumberSpiral.new(5).diagonal_sum.should eq(101)
    end

    it "returns correct sum for 7×7 spiral" do
      NumberSpiral.new(7).diagonal_sum.should eq(261)
    end
  end

  describe ".solve" do
    it "solves for default size 1001" do
      result = NumberSpiral.solve
      result.should be > 1000000
      result.should be < 1000000000
    end

    it "solves for custom size" do
      NumberSpiral.solve(5).should eq(101)
    end

    it "returns Int64 type" do
      NumberSpiral.solve(3).should be_a(Int64)
    end
  end
end
