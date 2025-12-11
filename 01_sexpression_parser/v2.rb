#!/usr/bin/env ruby

# Parseur générique de S-expressions
class SExpressionParser
  def initialize(node_types_module)
    @nodes = node_types_module
  end

  def parse(input)
    @tokens = tokenize(input)
    @pos = 0
    parse_expr
  end

  private

  def tokenize(input)
    # Supprime les commentaires (tout ce qui suit ";;" jusqu'à la fin de ligne)
    input_without_comments = input.gsub(/;;.*$/, '')
    input_without_comments.scan(/"[^"]*"|\(|\)|[^\s()]+/)
  end

  def current_token
    @tokens[@pos]
  end

  def consume
    token = current_token
    @pos += 1
    token
  end

  def parse_expr
    token = current_token

    return nil if token.nil?

    if token == '('
      parse_list
    else
      parse_atom
    end
  end

  def parse_list
    consume # consomme '('
    elements = []

    while current_token && current_token != ')'
      elements << parse_expr
    end

    raise "Expected ')'" if current_token != ')'
    consume # consomme ')'

    # Crée un nœud en fonction du premier élément
    return @nodes::EmptyList.new if elements.empty?

    head = elements.first
    tail = elements[1..]

    # Si le head est un symbole/atom, on essaie de créer le nœud correspondant
    if head.is_a?(@nodes::Symbol) || head.is_a?(@nodes::Atom)
      node_name = head.value.capitalize

      # Essaie de trouver une classe correspondante dans le module
      if @nodes.const_defined?(node_name)
        node_class = @nodes.const_get(node_name)
        return node_class.new(*tail)
      end
    end

    # Sinon, crée une liste générique
    @nodes::List.new(elements)
  end

  def parse_atom
    token = consume

    # Essaie de parser comme nombre
    if token =~ /^-?\d+$/
      return @nodes::Number.new(token.to_i)
    elsif token =~ /^-?\d+\.\d+$/
      return @nodes::Number.new(token.to_f)
    end

    # Essaie de parser comme string (entre guillemets)
    if token =~ /^".*"$/
      return @nodes::String.new(token[1...-1])
    end

    # Sinon, c'est un symbole
    @nodes::Symbol.new(token)
  end
end

# =============================================================================
# EXEMPLE D'UTILISATION : Définir un module avec les types de nœuds
# =============================================================================

module MathAST
  # Classe de base pour tous les nœuds
  class Node
    def inspect
      to_s
    end

    # Méthode abstraite à implémenter dans chaque sous-classe
    def to_sexp
      raise NotImplementedError, "#{self.class} must implement to_sexp"
    end
  end

  # Nœuds atomiques
  class Number < Node
    attr_reader :value

    def initialize(value)
      @value = value
    end

    def to_s
      "Number(#{@value})"
    end

    def to_sexp
      @value.to_s
    end
  end

  class Symbol < Node
    attr_reader :value

    def initialize(value)
      @value = value
    end

    def to_s
      "Symbol(#{@value})"
    end

    def to_sexp
      @value
    end
  end

  class String < Node
    attr_reader :value

    def initialize(value)
      @value = value
    end

    def to_s
      "String(\"#{@value}\")"
    end

    def to_sexp
      "\"#{@value}\""
    end
  end

  class Atom < Node
    attr_reader :value

    def initialize(value)
      @value = value
    end

    def to_s
      "Atom(#{@value})"
    end

    def to_sexp
      @value
    end
  end

  # Nœuds de structure
  class List < Node
    attr_reader :elements

    def initialize(elements)
      @elements = elements
    end

    def to_s
      "List[#{@elements.map(&:to_s).join(', ')}]"
    end

    def to_sexp
      "(#{@elements.map(&:to_sexp).join(' ')})"
    end
  end

  class EmptyList < Node
    def to_s
      "EmptyList"
    end

    def to_sexp
      "()"
    end
  end

  # Nœuds d'opérations mathématiques
  class Plus < Node
    attr_reader :left, :right

    def initialize(left, right)
      @left = left
      @right = right
    end

    def to_s
      "Plus(#{@left}, #{@right})"
    end

    def to_sexp
      "(plus #{@left.to_sexp} #{@right.to_sexp})"
    end
  end

  class Minus < Node
    attr_reader :left, :right

    def initialize(left, right)
      @left = left
      @right = right
    end

    def to_s
      "Minus(#{@left}, #{@right})"
    end

    def to_sexp
      "(minus #{@left.to_sexp} #{@right.to_sexp})"
    end
  end

  class Mult < Node
    attr_reader :left, :right

    def initialize(left, right)
      @left = left
      @right = right
    end

    def to_s
      "Mult(#{@left}, #{@right})"
    end

    def to_sexp
      "(mult #{@left.to_sexp} #{@right.to_sexp})"
    end
  end

  class Div < Node
    attr_reader :left, :right

    def initialize(left, right)
      @left = left
      @right = right
    end

    def to_s
      "Div(#{@left}, #{@right})"
    end

    def to_sexp
      "(div #{@left.to_sexp} #{@right.to_sexp})"
    end
  end

  # Nœud pour définir des variables
  class Define < Node
    attr_reader :name, :value

    def initialize(name, value)
      @name = name
      @value = value
    end

    def to_s
      "Define(#{@name}, #{@value})"
    end

    def to_sexp
      "(define #{@name.to_sexp} #{@value.to_sexp})"
    end
  end
end

# =============================================================================
# TESTS
# =============================================================================

parser = SExpressionParser.new(MathAST)

puts "=== Test 1: Nombres simples ==="
ast = parser.parse("42")
puts ast
puts

puts "=== Test 2: Addition simple ==="
ast = parser.parse("(plus 2 3)")
puts ast
puts

puts "=== Test 3: Opérations imbriquées ==="
ast = parser.parse("(mult (plus 2 3) (minus 10 4))")
puts ast
puts

puts "=== Test 4: Définition de variable ==="
ast = parser.parse("(define x 42)")
puts ast
puts

puts "=== Test 5: Expression complexe ==="
ast = parser.parse("(div (plus 100 (mult 2 3)) (minus 20 4))")
puts ast
puts

puts "=== Test 6: Liste générique (opération non définie) ==="
ast = parser.parse("(foo bar baz)")
puts ast
puts

puts "=== Test 7: String ==="
ast = parser.parse('(define message "Hello World")')
puts ast
puts

puts "=== Test 8: Commentaires ==="
ast = parser.parse('
  ;; Ceci est un commentaire
  (plus 2 3) ;; addition simple
  ')
puts ast
puts

puts "=== Test 9: Round-trip (parse -> serialize) ==="
input = "(mult (plus 2 3) (minus 10 4))"
puts "Input:  #{input}"
ast = parser.parse(input)
puts "AST:    #{ast}"
output = ast.to_sexp
puts "Output: #{output}"
puts

puts "=== Test 10: Vérification round-trip ==="
ast2 = parser.parse(output)
output2 = ast2.to_sexp
puts "Parsing à nouveau: #{output == output2 ? '✓ Identique' : '✗ Différent'}"
