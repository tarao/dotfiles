require 'irb/completion'
require 'nkf'
require 'pp'
IRB.conf[:SAVE_HISTORY] = 100000

begin
  require 'rubygems'
  require 'wirble'

  Wirble.init
  Wirble.colorize
rescue LoadError => err
  warn("Couldn't load Wirble: #{err}")
end

module Kernel
  def r(arg)
    puts(NKF.nkf('-w', `refe #{arg}`))
  end
  private :r
end

class Module
  def r(method = nil)
    if method
      if instance_methods(false).include?(method.to_s)
        puts(NKF.nkf('-w', `refe #{self}##{method}`))
      else
        super
      end
    else
      puts(NKF.nkf('-w', `refe #{self}`))
    end
  end
end
