require 'fizzbuzz'

describe 'FizzBuzz' do
  context 'knows that a number is divisible by' do
    it '3' do
      expect(is_divisible_by_three?(3)).to be_true
    end
    it '5' do
      expect(is_divisible_by_five?(5)).to be_true
    end
    it '15' do
      expect(is_divisible_by_fifteen?(15)).to be_true
    end
  end
  context 'knows that a number is not divisible by' do
    it '3' do
      expect(is_divisible_by_three?(1)).not_to be_true
    end
    it '5' do
      expect(is_divisible_by_five?(1)).not_to be_true
    end
    it '15' do
      expect(is_divisible_by_fifteen?(1)).not_to be_true
    end
  end
  context 'while playing the game it returns' do
    it 'the number' do
      expect(fizzbuzz(1)).to eq 1
    end
    it 'Fizz' do
      expect(fizzbuzz(3)).to eq 'Fizz'
    end
    it 'Buzz' do
      expect(fizzbuzz(5)).to eq 'Buzz'
    end
    it 'FizzBuzz' do
      expect(fizzbuzz(15)).to eq 'FizzBuzz'
    end
  end
end
