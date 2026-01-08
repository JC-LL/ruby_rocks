# Token types
class Token
  attr_reader :type, :value

  def initialize(type, value = nil)
    @type = type
    @value = value
  end

  def to_s
    @value ? "#{@type}(#{@value})" : @type.to_s
  end
end

# Lexer simplifié (à adapter selon votre implémentation)
class Lexer
  KEYWORDS = {
    'sin' => :SIN,
    'cos' => :COS,
    'tan' => :TAN,
    'ln'  => :LN,
    'exp' => :EXP
  }

  def initialize(input)
    @input = input
    @pos = 0
    @tokens = []
    tokenize
  end

  def tokenize
    while @pos < @input.length
      case @input[@pos]
      when /\s/
        @pos += 1
      when /\d/
        num = ''
        while @pos < @input.length && @input[@pos] =~ /\d/
          num << @input[@pos]
          @pos += 1
        end
        if @pos < @input.length && @input[@pos] == '.'
          num << '.'
          @pos += 1
          while @pos < @input.length && @input[@pos] =~ /\d/
            num << @input[@pos]
            @pos += 1
          end
        end
        @tokens << Token.new(:NUMBER, num.to_f)
      when /[a-zA-Z]/
        ident = ''
        while @pos < @input.length && @input[@pos] =~ /[a-zA-Z]/
          ident << @input[@pos]
          @pos += 1
        end
        if KEYWORDS.key?(ident)
          @tokens << Token.new(KEYWORDS[ident])
        else
          @tokens << Token.new(:IDENT, ident)
        end
      when '+'
        @tokens << Token.new(:PLUS)
        @pos += 1
      when '-'
        @tokens << Token.new(:MINUS)
        @pos += 1
      when '*'
        @tokens << Token.new(:MULT)
        @pos += 1
      when '/'
        @tokens << Token.new(:DIV)
        @pos += 1
      when '('
        @tokens << Token.new(:LPAREN)
        @pos += 1
      when ')'
        @tokens << Token.new(:RPAREN)
        @pos += 1
      when '^'
        @tokens << Token.new(:POW)
        @pos += 1
      else
        raise "Caractère invalide: #{@input[@pos]}"
      end
    end
    @tokens << Token.new(:EOF)
  end

  def tokens
    @tokens
  end
end

# Parser récursif descendant
class Parser
  def initialize(lexer)
    @tokens = lexer.tokens
    @pos = 0
  end

  # Méthodes de consommation de tokens
  def show_next
    @tokens[@pos]
  end

  def accept_it
    token = @tokens[@pos]
    @pos += 1
    token
  end

  def accept(expected_type)
    if show_next.type == expected_type
      accept_it
    else
      raise "Token inattendu: #{show_next.type}, attendu: #{expected_type}"
    end
  end

  # Règles de grammaire avec priorité

  # expression -> term (('+' | '-') term)*
  def parse_expression
    node = parse_term

    while show_next.type == :PLUS || show_next.type == :MINUS
      token = accept_it
      right = parse_term
      node = { type: :BINARY_OP, op: token.type, left: node, right: right }
    end

    node
  end

  # term -> factor (('*' | '/') factor)*
  def parse_term
    node = parse_factor

    while show_next.type == :MULT || show_next.type == :DIV
      token = accept_it
      right = parse_factor
      node = { type: :BINARY_OP, op: token.type, left: node, right: right }
    end

    node
  end

  # factor -> power ('^' factor)?
  def parse_factor
    node = parse_power

    if show_next.type == :POW
      token = accept_it
      right = parse_factor
      node = { type: :BINARY_OP, op: token.type, left: node, right: right }
    end

    node
  end

  # power -> unary
  def parse_power
    parse_unary
  end

  # unary -> ('+' | '-') unary | primary
  def parse_unary
    if show_next.type == :PLUS || show_next.type == :MINUS
      token = accept_it
      operand = parse_unary
      { type: :UNARY_OP, op: token.type, operand: operand }
    else
      parse_primary
    end
  end

  # primary -> NUMBER | IDENT | function | '(' expression ')'
  def parse_primary
    case show_next.type
    when :NUMBER
      token = accept(:NUMBER)
      { type: :NUMBER, value: token.value }

    when :IDENT
      token = accept(:IDENT)
      { type: :VARIABLE, name: token.value }

    when :SIN, :COS, :TAN, :LN, :EXP
      token = accept_it
      accept(:LPAREN)
      arg = parse_expression
      accept(:RPAREN)
      { type: :FUNCTION, func: token.type, arg: arg }

    when :LPAREN
      accept(:LPAREN)
      expr = parse_expression
      accept(:RPAREN)
      expr

    else
      raise "Token inattendu dans primary: #{show_next.type}"
    end
  end

  def parse
    parse_expression
  end
end

# Fonction d'évaluation (optionnelle)
class Evaluator
  def evaluate(node, vars = {})
    case node[:type]
    when :NUMBER
      node[:value]

    when :VARIABLE
      vars[node[:name]] || 0

    when :UNARY_OP
      operand = evaluate(node[:operand], vars)
      case node[:op]
      when :PLUS then operand
      when :MINUS then -operand
      end

    when :BINARY_OP
      left = evaluate(node[:left], vars)
      right = evaluate(node[:right], vars)
      case node[:op]
      when :PLUS then left + right
      when :MINUS then left - right
      when :MULT then left * right
      when :DIV then left / right
      when :POW then left ** right
      end

    when :FUNCTION
      arg = evaluate(node[:arg], vars)
      case node[:func]
      when :SIN then Math.sin(arg)
      when :COS then Math.cos(arg)
      when :TAN then Math.tan(arg)
      when :LN then Math.log(arg)
      when :EXP then Math.exp(arg)
      end
    end
  end
end

# Exemple d'utilisation
if __FILE__ == $0
  # Test avec différentes expressions
  tests = [
    "2 + 3 * 4",
    "(2 + 3) * 4",
    "sin(x) + cos(y)",
    "exp(2) * 3",
    "2^3 + 4",
    "-5 + 3 * 2",
    "ln(exp(5))"
  ]

  tests.each do |expr|
    puts "\nExpression: #{expr}"
    begin
      lexer = Lexer.new(expr)
      parser = Parser.new(lexer)
      ast = parser.parse
      puts "AST: #{ast}"

      # Évaluation avec des variables
      evaluator = Evaluator.new
      vars = { 'x' => Math::PI/2, 'y' => 0 }
      result = evaluator.evaluate(ast, vars)
      puts "Résultat (avec x=π/2, y=0): #{result}" #if expr.include?('x') || expr.include?('y')
    rescue => e
      puts "Erreur: #{e.message}"
    end
  end
end
