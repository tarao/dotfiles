require 'irb/completion'
require 'kconv'
require 'pp'
IRB.conf[:SAVE_HISTORY] = 100000

module Kernel
  def r(arg)
    puts(`refe #{arg}`.toutf8)
  end
  private :r
end

class Module
  def r(method = nil)
    if method
      if instance_methods(false).include?(method.to_s)
        puts(`refe #{self}##{method}`.toutf8)
      else
        super
      end
    else
      puts(`refe #{self}`.toutf8)
    end
  end
end
