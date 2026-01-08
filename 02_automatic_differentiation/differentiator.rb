require_relative "proto_0"

# Formatter pour convertir AST en chaîne
class ASTFormatter
  def to_string(node, parent_precedence = 0)
    case node[:type]
    when :NUMBER
      format_number(node[:value])
    when :VARIABLE
      node[:name]
    when :UNARY_OP
      format_unary(node, parent_precedence)
    when :BINARY_OP
      format_binary(node, parent_precedence)
    when :FUNCTION
      format_function(node)
    else
      raise "Type de nœud non supporté: #{node[:type]}"
    end
  end

  private

  def format_number(value)
    # Affiche les nombres entiers sans décimale
    value.to_i == value ? value.to_i.to_s : value.to_s
  end

  def format_unary(node, parent_precedence)
    op_str = node[:op] == :MINUS ? "-" : "+"
    operand_str = to_string(node[:operand], 5) # Haute priorité pour les unaires

    # On n'affiche pas le + unaire explicite
    if op_str == "+"
      operand_str
    else
      # Ajouter des parenthèses si nécessaire (ex: -(a+b))
      case node[:operand][:type]
      when :BINARY_OP
        "(#{op_str}#{operand_str})"
      else
        "#{op_str}#{operand_str}"
      end
    end
  end

  def format_binary(node, parent_precedence)
    precedence = operator_precedence(node[:op])
    associativity = operator_associativity(node[:op])

    left_str = to_string(node[:left],
                        precedence + (associativity == :right ? 1 : 0))
    right_str = to_string(node[:right],
                         precedence + (associativity == :left ? 1 : 0))

    op_symbol = operator_symbol(node[:op])
    str = "#{left_str} #{op_symbol} #{right_str}"

    # Ajouter des parenthèses si nécessaire
    if precedence < parent_precedence
      "(#{str})"
    else
      str
    end
  end

  def format_function(node)
    func_name = case node[:func]
                when :SIN then "sin"
                when :COS then "cos"
                when :TAN then "tan"
                when :LN then "ln"
                when :EXP then "exp"
                else node[:func].to_s.downcase
                end
    "#{func_name}(#{to_string(node[:arg])})"
  end

  def operator_precedence(op)
    case op
    when :POW then 4
    when :MULT, :DIV then 3
    when :PLUS, :MINUS then 2
    else 1
    end
  end

  def operator_associativity(op)
    case op
    when :POW then :right
    when :PLUS, :MINUS, :MULT, :DIV then :left
    else :left
    end
  end

  def operator_symbol(op)
    case op
    when :PLUS then "+"
    when :MINUS then "-"
    when :MULT then "*"
    when :DIV then "/"
    when :POW then "^"
    else op.to_s
    end
  end
end

# Differentiator modifié pour utiliser le formatter externe
class Differentiator
  attr_reader :formatter

  def initialize(formatter = ASTFormatter.new)
    @formatter = formatter
  end

  # Différencie un AST par rapport à x
  def differentiate(node)
    case node[:type]
    when :NUMBER
      # d/dx [constante] = 0
      { type: :NUMBER, value: 0 }

    when :VARIABLE
      # d/dx [x] = 1, d/dx [y] = 0
      if node[:name] == 'x'
        { type: :NUMBER, value: 1 }
      else
        { type: :NUMBER, value: 0 }
      end

    when :UNARY_OP
      # d/dx [+u] = +du/dx, d/dx [-u] = -du/dx
      du = differentiate(node[:operand])
      { type: :UNARY_OP, op: node[:op], operand: du }

    when :BINARY_OP
      u = node[:left]
      v = node[:right]
      du = differentiate(u)
      dv = differentiate(v)

      case node[:op]
      when :PLUS
        # d/dx [u + v] = du/dx + dv/dx
        { type: :BINARY_OP, op: :PLUS, left: du, right: dv }

      when :MINUS
        # d/dx [u - v] = du/dx - dv/dx
        { type: :BINARY_OP, op: :MINUS, left: du, right: dv }

      when :MULT
        # d/dx [u * v] = u * dv/dx + v * du/dx (règle du produit)
        left_term = { type: :BINARY_OP, op: :MULT, left: u, right: dv }
        right_term = { type: :BINARY_OP, op: :MULT, left: v, right: du }
        { type: :BINARY_OP, op: :PLUS, left: left_term, right: right_term }

      when :DIV
        # d/dx [u / v] = (v * du/dx - u * dv/dx) / v^2 (règle du quotient)
        numerator_left = { type: :BINARY_OP, op: :MULT, left: v, right: du }
        numerator_right = { type: :BINARY_OP, op: :MULT, left: u, right: dv }
        numerator = { type: :BINARY_OP, op: :MINUS, left: numerator_left, right: numerator_right }
        denominator = { type: :BINARY_OP, op: :POW, left: v, right: { type: :NUMBER, value: 2 } }
        { type: :BINARY_OP, op: :DIV, left: numerator, right: denominator }

      when :POW
        # Deux cas: u^constante et u^v (cas général)
        if v[:type] == :NUMBER
          # d/dx [u^n] = n * u^(n-1) * du/dx (règle de la puissance)
          n = v[:value]
          n_minus_1 = n - 1

          # n * u^(n-1)
          u_pow_n_minus_1 = {
            type: :BINARY_OP,
            op: :POW,
            left: u,
            right: { type: :NUMBER, value: n_minus_1 }
          }

          left_part = {
            type: :BINARY_OP,
            op: :MULT,
            left: { type: :NUMBER, value: n },
            right: u_pow_n_minus_1
          }

          {
            type: :BINARY_OP,
            op: :MULT,
            left: left_part,
            right: du
          }
        else
          # Cas général: d/dx [u^v] = u^v * (v * du/dx / u + ln(u) * dv/dx)
          # Utilise la formule: d/dx [u^v] = u^v * (v' * ln(u) + v * u' / u)

          # u^v
          u_pow_v = { type: :BINARY_OP, op: :POW, left: u, right: v }

          # v * du/dx / u
          v_times_du = { type: :BINARY_OP, op: :MULT, left: v, right: du }
          term1 = { type: :BINARY_OP, op: :DIV, left: v_times_du, right: u }

          # ln(u) * dv/dx
          ln_u = { type: :FUNCTION, func: :LN, arg: u }
          term2 = { type: :BINARY_OP, op: :MULT, left: ln_u, right: dv }

          # (term1 + term2)
          sum_terms = { type: :BINARY_OP, op: :PLUS, left: term1, right: term2 }

          # u^v * (term1 + term2)
          { type: :BINARY_OP, op: :MULT, left: u_pow_v, right: sum_terms }
        end

      end

    when :FUNCTION
      arg = node[:arg]
      d_arg = differentiate(arg)

      case node[:func]
      when :SIN
        # d/dx [sin(u)] = cos(u) * du/dx
        cos_u = { type: :FUNCTION, func: :COS, arg: arg }
        { type: :BINARY_OP, op: :MULT, left: cos_u, right: d_arg }

      when :COS
        # d/dx [cos(u)] = -sin(u) * du/dx
        sin_u = { type: :FUNCTION, func: :SIN, arg: arg }
        neg_sin_u = { type: :UNARY_OP, op: :MINUS, operand: sin_u }
        { type: :BINARY_OP, op: :MULT, left: neg_sin_u, right: d_arg }

      when :TAN
        # d/dx [tan(u)] = sec²(u) * du/dx = (1 + tan²(u)) * du/dx
        tan_u = { type: :FUNCTION, func: :TAN, arg: arg }
        tan_squared = { type: :BINARY_OP, op: :POW, left: tan_u, right: { type: :NUMBER, value: 2 } }
        one_plus_tan2 = { type: :BINARY_OP, op: :PLUS, left: { type: :NUMBER, value: 1 }, right: tan_squared }
        { type: :BINARY_OP, op: :MULT, left: one_plus_tan2, right: d_arg }

      when :LN
        # d/dx [ln(u)] = (1/u) * du/dx
        one_over_u = {
          type: :BINARY_OP,
          op: :DIV,
          left: { type: :NUMBER, value: 1 },
          right: arg
        }
        { type: :BINARY_OP, op: :MULT, left: one_over_u, right: d_arg }

      when :EXP
        # d/dx [exp(u)] = exp(u) * du/dx
        exp_u = { type: :FUNCTION, func: :EXP, arg: arg }
        { type: :BINARY_OP, op: :MULT, left: exp_u, right: d_arg }
      end

    else
      raise "Type de nœud non reconnu pour la différentiation: #{node[:type]}"
    end
  end

  # Simplifie l'AST
  def simplify(node)
    case node[:type]
    when :NUMBER
      node

    when :VARIABLE
      node

    when :UNARY_OP
      operand = simplify(node[:operand])

      # Simplifications spécifiques
      if operand[:type] == :NUMBER
        # Évaluer les opérations unaires sur des constantes
        value = operand[:value]
        case node[:op]
        when :PLUS
          { type: :NUMBER, value: value }
        when :MINUS
          { type: :NUMBER, value: -value }
        end
      else
        # -(-x) = x
        if node[:op] == :MINUS && operand[:type] == :UNARY_OP && operand[:op] == :MINUS
          return simplify(operand[:operand])
        end
        { type: :UNARY_OP, op: node[:op], operand: operand }
      end

    when :BINARY_OP
      left = simplify(node[:left])
      right = simplify(node[:right])

      # Si les deux côtés sont des nombres, évaluer
      if left[:type] == :NUMBER && right[:type] == :NUMBER
        left_val = left[:value]
        right_val = right[:value]

        result = case node[:op]
                when :PLUS then left_val + right_val
                when :MINUS then left_val - right_val
                when :MULT then left_val * right_val
                when :DIV then left_val / right_val
                when :POW then left_val ** right_val
                end

        return { type: :NUMBER, value: result }
      end

      # Simplifications algébriques
      case node[:op]
      when :PLUS
        # 0 + x = x
        if left[:type] == :NUMBER && left[:value] == 0
          return right
        end
        # x + 0 = x
        if right[:type] == :NUMBER && right[:value] == 0
          return left
        end
        # x + (-y) = x - y
        if right[:type] == :UNARY_OP && right[:op] == :MINUS
          return simplify({
            type: :BINARY_OP,
            op: :MINUS,
            left: left,
            right: right[:operand]
          })
        end

      when :MINUS
        # x - 0 = x
        if right[:type] == :NUMBER && right[:value] == 0
          return left
        end
        # 0 - x = -x
        if left[:type] == :NUMBER && left[:value] == 0
          return { type: :UNARY_OP, op: :MINUS, operand: right }
        end
        # x - x = 0
        if deep_equal(left, right)
          return { type: :NUMBER, value: 0 }
        end
        # x - (-y) = x + y
        if right[:type] == :UNARY_OP && right[:op] == :MINUS
          return simplify({
            type: :BINARY_OP,
            op: :PLUS,
            left: left,
            right: right[:operand]
          })
        end

      when :MULT
        # 0 * x = 0
        if (left[:type] == :NUMBER && left[:value] == 0) ||
           (right[:type] == :NUMBER && right[:value] == 0)
          return { type: :NUMBER, value: 0 }
        end
        # 1 * x = x
        if left[:type] == :NUMBER && left[:value] == 1
          return right
        end
        # x * 1 = x
        if right[:type] == :NUMBER && right[:value] == 1
          return left
        end
        # (-1) * x = -x
        if left[:type] == :NUMBER && left[:value] == -1
          return simplify({ type: :UNARY_OP, op: :MINUS, operand: right })
        end
        # x * (-1) = -x
        if right[:type] == :NUMBER && right[:value] == -1
          return simplify({ type: :UNARY_OP, op: :MINUS, operand: left })
        end

      when :DIV
        # 0 / x = 0 (sauf si x = 0)
        if left[:type] == :NUMBER && left[:value] == 0
          return { type: :NUMBER, value: 0 }
        end
        # x / 1 = x
        if right[:type] == :NUMBER && right[:value] == 1
          return left
        end
        # x / x = 1
        if deep_equal(left, right)
          return { type: :NUMBER, value: 1 }
        end

      when :POW
        # x^0 = 1
        if right[:type] == :NUMBER && right[:value] == 0
          return { type: :NUMBER, value: 1 }
        end
        # x^1 = x
        if right[:type] == :NUMBER && right[:value] == 1
          return left
        end
        # 1^x = 1
        if left[:type] == :NUMBER && left[:value] == 1
          return { type: :NUMBER, value: 1 }
        end
        # 0^x = 0 (pour x > 0, mais on simplifie quand même)
        if left[:type] == :NUMBER && left[:value] == 0
          return { type: :NUMBER, value: 0 }
        end
      end

      # Aucune simplification possible
      { type: :BINARY_OP, op: node[:op], left: left, right: right }

    when :FUNCTION
      arg = simplify(node[:arg])

      # Si l'argument est un nombre, évaluer la fonction
      if arg[:type] == :NUMBER
        value = arg[:value]
        begin
          result = case node[:func]
                  when :SIN then Math.sin(value)
                  when :COS then Math.cos(value)
                  when :TAN then Math.tan(value)
                  when :LN then Math.log(value)
                  when :EXP then Math.exp(value)
                  end
          return { type: :NUMBER, value: result }
        rescue Math::DomainError
          # Garder la fonction si évaluation impossible (ex: ln(-1))
        end
      end

      { type: :FUNCTION, func: node[:func], arg: arg }
    end
  end

  # Compare deux AST pour l'égalité structurelle
  def deep_equal(a, b)
    return false if a[:type] != b[:type]

    case a[:type]
    when :NUMBER
      a[:value] == b[:value]
    when :VARIABLE
      a[:name] == b[:name]
    when :UNARY_OP
      a[:op] == b[:op] && deep_equal(a[:operand], b[:operand])
    when :BINARY_OP
      a[:op] == b[:op] && deep_equal(a[:left], b[:left]) && deep_equal(a[:right], b[:right])
    when :FUNCTION
      a[:func] == b[:func] && deep_equal(a[:arg], b[:arg])
    else
      false
    end
  end
end

# Exemple d'utilisation
if __FILE__ == $0
  # Créer le formatter
  formatter = ASTFormatter.new

  # Test avec différentes expressions
  tests = [
    "x^2",
    "sin(x)",
    "3*x^2 + 2*x + 1",
    "exp(2*x)",
    "ln(x)",
    "x*sin(x)",
    "(x^2 + 1)/(x - 1)",
    "tan(x)",
    "x^3",
    "2^x"
  ]

  puts "TEST DES DÉRIVÉES"
  puts "=" * 60

  tests.each do |expr|
    puts "\nExpression: f(x) = #{expr}"

    begin
      # Parser l'expression
      lexer = Lexer.new(expr)
      parser = Parser.new(lexer)
      ast = parser.parse

      puts "AST original: #{formatter.to_string(ast)}"

      # Calculer la dérivée
      differentiator = Differentiator.new(formatter)
      derivative_ast = differentiator.differentiate(ast)

      puts "Dérivée brute: #{formatter.to_string(derivative_ast)}"

      # Simplifier
      simplified = differentiator.simplify(derivative_ast)
      puts "Dérivée simplifiée: f'(x) = #{formatter.to_string(simplified)}"

    rescue => e
      puts "Erreur: #{e.message}"
      puts e.backtrace if ENV['DEBUG']
    end
  end

  # Test numérique
  puts "\n\n" + "=" * 60
  puts "TEST NUMÉRIQUE"

  test_expr = "x^2 * sin(x)"
  puts "\nExpression: f(x) = #{test_expr}"

  lexer = Lexer.new(test_expr)
  parser = Parser.new(lexer)
  ast = parser.parse

  differentiator = Differentiator.new(formatter)
  derivative_ast = differentiator.differentiate(ast)
  simplified_derivative = differentiator.simplify(derivative_ast)

  puts "Dérivée: f'(x) = #{formatter.to_string(simplified_derivative)}"

  # Évaluer en un point
  evaluator = Evaluator.new
  x_value = Math::PI / 3
  vars = { 'x' => x_value }

  f_value = evaluator.evaluate(ast, vars)
  f_prime_value = evaluator.evaluate(simplified_derivative, vars)

  puts "\nÉvaluation en x = #{x_value}:"
  puts "f(#{x_value}) = #{f_value.round(6)}"
  puts "f'(#{x_value}) = #{f_prime_value.round(6)}"

  # Vérification par différence finie
  h = 0.0001
  vars_h = { 'x' => x_value + h }
  f_value_h = evaluator.evaluate(ast, vars_h)
  derivative_finite_diff = (f_value_h - f_value) / h

  puts "\nVérification par différence finie (h = #{h}):"
  puts "f'(#{x_value}) ≈ #{(derivative_finite_diff).round(6)}"
  puts "Différence: #{(f_prime_value - derivative_finite_diff).abs.round(10)}"
end
