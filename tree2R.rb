#!/usr/bin/env ruby -w

# == Synopsis
#
# Turns tree into R code
#

require 'rubygems'
require 'bio'
#require 'rsruby'
require 'optparse'
require 'ostruct'
require 'set'
require 'logger'

$LOG = Logger.new(STDERR)
$LOG.level = Logger::WARN
$LOG.debug "Created logger"

# Load in virus-specific data
before = ""
after = ".virus"
entries = Dir.getwd.split(/\//).reverse
$THIS_DIR = entries[0]
possibles = entries.map do |entry|
  tmp=before+entry+after
  before += "../"
  tmp
end

ended = false
files = possibles.reject do |file|
  ended |= !File.readable?(file)
end

files.reverse!
if files.empty?
  $LOG.fatal "Defaults file '$(files[0])' not found in this directory"
  raise "Defaults file '$(files[0])' not found in this directory"
end

files.each do |file|
  File.open(file) do |data|
    defHash = Hash.new
    data.each_line do |input|
      input.strip!
      unless (input.empty? or (input[0] == "#"[0]))
	line = input.split(/:/)
	defHash[line[0]] = line[1]
      end
    end
    $VIRUS = defHash["VIRUS"] if defHash.key?("VIRUS")
    $SEROTYPE = defHash["SEROTYPE"] if defHash.key?("SEROTYPE")
  end
end

class Tree
  attr_reader :leftTree, :rightTree, :token, :height

  def initialize(string)
    @leftTree = nil
    @rightTree = nil
    @token = nil
    depth = 0
    index = 0
    foundLeft = false
    foundRight = false
    if (string.start_with?("("))
      # new branches
      index = 1
      depth = 1
      
      startLeft = index
      while (!foundLeft)
	comma = (string[index..-1] =~ /,/)
	leftB = (string[index..-1] =~ /[\(\[\{]/)
	rightB = (string[index..-1] =~ /[\)\]\}]/)
	if (comma.nil?)
	  $LOG.fatal "Error in tree, invalid format - missing ,"
	  raise
	end
	if (rightB.nil?)
	  $LOG.fatal "Error in tree, invalid format - missing ) ] }"
	  raise
	end
	if (((rightB < comma) or (depth > 1)) and
	    (leftB.nil? or (rightB < leftB)))
	  # right bracket next
	  index += rightB + 1
	  depth -= 1
	  if (depth == 0)
	    $LOG.fatal "Error in tree, invalid format - too many ) ] }"
	    raise
	  end
	else
	  if (leftB.nil? or ((depth == 1) and (leftB > comma)))
	    # comma at depth = 1, so done!
	    index += comma + 1
	    endLeft = index - 2
	    foundLeft = true
	    @leftTree = Tree.new(string[startLeft..endLeft])
	  else
	    # left bracket next
	    index += leftB + 1
	    depth += 1
	  end
	end
      end
      
      startRight = index
      while (!foundRight)
	leftB = (string[index..-1] =~ /[\(\[\{]/)
	rightB = (string[index..-1] =~ /[\)\]\}]/)
	if (rightB.nil?)
	  $LOG.fatal "Error in tree, invalid format - missing ) ] }"
	  raise
	end
	if (leftB.nil? or (rightB < leftB))
	  # right bracket next
	  index += rightB + 1
	  depth -= 1
	  if (depth == 0)
	    # final right bracket, so done!
	    endRight = index - 2
	    foundRight = true
	    @rightTree = Tree.new(string[startRight..endRight])
	    place = (string[endRight..-1] =~ /:/)
	    if (place.nil?)
	      @height = nil
	    else
	      @height = string[(endRight+place+1)..-1].to_f
	    end
	  end
	else
	  # left bracket next
	  index += leftB + 1
	  depth += 1
	end
      end
    else
      # leaf, take token (up to first [)
      sqbracket = index + (string[index..-1] =~ /\[/)
      @token = string[0, sqbracket]
      place = (string[sqbracket..-1] =~ /:/)
      if (place.nil?)
	@height = nil
      else
	@height = string[(sqbracket+place+1)..-1].to_f
      end
    end
  end

  def extend(height)
    unless (@height.nil? or height.nil?)
      @height += height
    else
      unless @token.nil?
	$LOG.error "Extending nil for #{@token}"
      else
	$LOG.debug "Empty (root?) branch has no height"
      end
    end
  end

  def collapse(excHash)
    res = self
    if @token.nil?
      left = @leftTree.collapse(excHash)
      right = @rightTree.collapse(excHash)
      res = self
      if left.nil? and right.nil?
	res = nil
      else
	if left.nil?
	  right.extend(@height)
	  res = right
	else
	  if right.nil?
	    left.extend(@height)
	    res = left
	  else
	    @leftTree = left
	    @rightTree = right
	  end
	end
      end
    else
      if excHash.key?(@token)
	res = nil
      end
    end
    res
  end

  def leaf?
    !@token.nil?
  end

  def display(incHash, excHash=Hash.new, level=0)
    text = ""
    if leaf?
      if (incHash.key?(@token))
	text += " " * level + incHash[@token] + "\n"
      else
	if (excHash.key?(@token))
	  text += " " * level + "[" + excHash[@token] + "]\n"
	else
	  $LOG.fatal "Wrong token found: #{@token}"
	  raise
	end
      end
    else
      text += " " * level + "|\n"
      text += @leftTree.display(incHash, excHash, level + 1)
      text += @rightTree.display(incHash, excHash, level + 1)
    end
    text
  end

  def outR(incHash, handle, labels=Array.new, full=Array.new)
    resHash = Hash.new
    label = nil
    if leaf?
      depth = 0
      if (incHash.key?(@token))
	if labels.empty?
	  # First go
	  label = "0001"
	else
	  # We've been this deep before, so increment label
	  label = labels[depth].next
	end
	handle.puts "phylo$challenge.#{label} <- " +
	  "(phylo$challenge == \"#{incHash[@token].sub(/^[^_]*_/,"")}\")"
	handle.puts "phylo$protective.#{label} <- " +
	  "(phylo$protective == \"#{incHash[@token].sub(/^[^_]*_/,"")}\")"
	handle.puts "phylo$control.#{label} <- " +
	  "xor(phylo$challenge.#{label}, phylo$protective.#{label})"
	handle.puts "phylo$both.#{label} <- " +
	  "(phylo$challenge.#{label} & phylo$protective.#{label})"
	handle.puts "phylo$either.#{label} <- " +
	  "(phylo$challenge.#{label} | phylo$protective.#{label})"
	labels[depth] = label
      else
	$LOG.fatal "Wrong token found: #{@token}"
      end
    else
      leftHash = @leftTree.outR(incHash, handle, labels, full)
      leftDepth = leftHash["depth"]
      leftLabel = leftHash["labels"][leftDepth]
      rightHash = @rightTree.outR(incHash, handle,
				  leftHash["labels"], leftHash["full"])
      rightDepth = rightHash["depth"]
      rightLabel = rightHash["labels"][rightDepth]
      labels = rightHash["labels"]
      full = rightHash["full"]
      depth = [leftDepth, rightDepth].max + 1
      if (labels.length > depth)
	# We've been this deep before, so increment label
	labelN = labels[depth].gsub(/[A-Z]/,"")
	labelL = labels[depth].gsub(/[0-9]/,"")
	label = labelN + labelL.next
      else
	# First go
	label = "#{depth}A"
      end
      labels[depth] = label
      handle.puts "phylo$challenge.#{label} <- " +
	"(phylo$challenge.#{leftLabel} | phylo$challenge.#{rightLabel})"
      handle.puts "phylo$protective.#{label} <- " +
	"(phylo$protective.#{leftLabel} | phylo$protective.#{rightLabel})"
      handle.puts "phylo$control.#{label} <- " +
	"xor(phylo$challenge.#{label}, phylo$protective.#{label})"
      handle.puts "phylo$both.#{label} <- " +
	"(phylo$challenge.#{label} & phylo$protective.#{label})"
      handle.puts "phylo$either.#{label} <- " +
	"(phylo$challenge.#{label} | phylo$protective.#{label})"
    end
    resHash["labels"] = labels
    resHash["depth"] = depth
    full.push(label)
    resHash["full"] = full
    resHash
  end

  def tree(incHash, handle, labels, excHash=Hash.new, level=0)
    allHash = incHash
    excHash.each { |key, value| allHash[key] = "(#{value})" }
    if (level == 0)
      handle.puts "#NEXUS"
      handle.puts
      handle.puts "Begin taxa;"
      handle.puts "  Dimensions ntax=#{allHash.length};"
      handle.puts "  Taxlabels"
      handle.puts "    " + allHash.values.join("\n    ")
      handle.puts "    ;"
      handle.puts "End;"
      handle.puts
      handle.puts "Begin trees;"
      handle.puts "  Translate"
      handle.puts allHash.map { |key, value| "    #{key} #{value}" }.join(",\n")
      handle.puts "    ;"
      handle.print "tree TREE1 = [&R] "
    end

    if leaf?
      if @height.nil?
	handle.print("#{@token}[&Control=\"#{labels.pop}\"]")
      else
	handle.print("#{@token}[&Control=\"#{labels.pop}\"]:#{@height}")
      end
    else
      handle.print("(")
      labels = @leftTree.tree(incHash, handle, labels, excHash, level + 1)
      handle.print(",")
      labels = @rightTree.tree(incHash, handle, labels, excHash, level + 1)
      if @height.nil?
	handle.print(")[&Control=\"#{labels.pop}\"]")
      else
	handle.print(")[&Control=\"#{labels.pop}\"]:#{@height}")
      end
    end

    if (level == 0)
      handle.puts ";"
      handle.puts "End;"
    end

    labels
  end
end

parser = OptionParser.new

parser.on("-l", "--log LEVEL",
	  "Set log level to D[EBUG], I[NFO], W[ARN], E[RROR] or F[ATAL]",
	  String) do |level|
  case level
  when "D", "DEBUG"
    $LOG.level = Logger::DEBUG
  when "I", "INFO"
    $LOG.level = Logger::INFO
  when "W", "WARN"
    $LOG.level = Logger::WARN
  when "E", "ERROR"
    $LOG.level = Logger::ERROR
  when "F", "FATAL"
    $LOG.level = Logger::FATAL
  else
    $LOG.fatal "Log level '#{level}' not recognised"
    raise "Log level '#{level}' not recognised"
  end
end

includes = Set.new
parser.on("-i", "--include S1[,S2...]", 
	  "Include sequences of serotype/virus(/starting text) S1[, S2...]",
	  Array) do |val|
  includes += val
end

excludes = Set.new
parser.on("-x", "--exclude S1[,S2...]", 
	  "Exclude sequences containing text E1[, E2...]",
	  Array) do |val|
  excludes += val
end

treefile = nil
parser.on("-t", "--tree file.trees", 
	  "Collect tree from file.trees",
	  String) do |val|
  if (File.readable?(val))
    treefile = val
  else
    $LOG.fatal "Phylogeny #{val} does not exist"
    raise
  end
end

poss_trees = Dir.glob("*.trees")
ordered_trees = poss_trees.reject { |tree| tree =~ /out.trees/ } +
  poss_trees.select { |tree| tree =~ /out\.trees/ }
  
default_tree = "../data/" + $SEROTYPE + ".trees"
unless File.readable?(default_tree)
  if poss_trees.empty?
    $LOG.error "Default tree #{default_tree} not found and no other trees found"
  else
    default_tree = ordered_trees[0]
  end
end

parser.separator ""
parser.on_tail("Tree processor for #{$VIRUS}\n\n")
parser.on_tail("Possible trees are: " + ordered_trees.join(",") + "\n\n")
parser.on_tail("Default arguments:")
parser.on_tail("  -i #{$SEROTYPE} -t #{default_tree} #{$THIS_DIR}")

begin
  parser.parse!(ARGV)
  if (treefile.nil?)
    treefile = default_tree
  end

  if (ARGV.length == 1)
    saveroot = ARGV.pop
  else
    saveroot = $THIS_DIR
  end
  savefile = "processed/#{saveroot}-phylo.R"
  savetreefile = "processed/#{saveroot}.trees"
  
  if includes.empty?
    if File.readable?("processed/#{saveroot}.viruses")
      File.open("processed/#{saveroot}.viruses") do |handle|
	handle.each_line do |input|
	  input.strip!
	  includes << input
	end
      end
    else
      if (includes.empty?)
	includes = Set.new.add($SEROTYPE)
      end
    end
  end

  unless ARGV.empty?
    $LOG.fatal "Unrecognised arguments: '#{ARGV.join(' ')}'" 
    raise "Unrecognised arguments: '#{ARGV.join(' ')}'" 
  end
rescue
  puts parser.to_s
  raise
end

Dir.mkdir("processed") unless File.directory?("processed")

includedSeq = Hash.new
excludedSeq = Hash.new
tree = nil
if File.readable?(treefile)
  foundTrees = false
  translating = true
  File.open(treefile) do |data|
    data.each_line do |input|
      input.strip!
      if (!foundTrees)
	if (input == "Translate")
	  foundTrees = true
	end
      else
	if (translating)
	  # reading translating table
	  if (input == ";")
	    # Translation table ends with ";"
	    translating = false
	  else
	    results = input.split.map { |i| i.split(/,/) }.flatten
	    if includes.any? { |inc| results[1].start_with?(inc) }
	      if excludes.any? { |exc| results[1] =~ Regexp.new(exc) }
		excludedSeq[results[0]] = results[1]
		$LOG.debug "Excluding #{results[0]} = #{results[1]}"
	      else
		includedSeq[results[0]] = results[1]
		$LOG.debug "Including #{results[0]} = #{results[1]}"
	      end
	    else
	      excludedSeq[results[0]] = results[1]
	      $LOG.debug "Excluding #{results[0]} = #{results[1]}"
	    end
	  end
	else
	  if (input == "End;")
	    # Tree list ends with "End;"
	    foundTrees = false
	  else
	    line = input.split
	    if (!line.empty?() and (line[0] == "tree"))
	      $LOG.info "This tree is called #{line[1]}"
	      if (line[4][0] != "("[0])
		$LOG.fatal "Tree not formatted as expected"
		raise
	      end
	      firstBranch = line[4].split(/;/)[0]
	      tree = Tree.new(firstBranch)
	    end
	  end
	end
      end
    end
  end
else
  $LOG.fatal "Tree '#{treefile}' not found"
  raise
end

$LOG.debug "\n" + tree.display(includedSeq, excludedSeq)
tree = tree.collapse(excludedSeq)
if (includedSeq.empty?)
  $LOG.fatal "No sequences to create phylogenetic R code for"
  raise
end

$LOG.info "Phylogeny of required sequences is:\n" + tree.display(includedSeq)
$LOG.info "#{includedSeq.length} sequences identified"

File.open(savefile, "w") do |handle|
  handle.puts <<BLOCK
phylo.add.#{saveroot.gsub(/-/, "_")}.controls <- function(phylo, all.terms)
  {
BLOCK

  if (includes.length != includedSeq.length)
    $LOG.warn "#{includes.length} include statements, " +
      "but #{includedSeq.length} viruses included"
    if (includes.length >= includedSeq.length)
      $LOG.warn "Missing sequence(s) are: #{includes.subtract(includedSeq.values).to_a.sort.join(', ')}"
      handle.puts <<BLOCK
print("Warning - removing data missing from phylogenies:")
print("Missing sequence(s) are: #{includes.subtract(includedSeq.values).to_a.sort.join(', ')}")
BLOCK
      includes.subtract(includedSeq.values).to_a.sort.each do |seq|
        handle.puts "phylo <- phylo[phylo$challenge != '#{seq.split(/_/, 2)[1]}',]"
        handle.puts "phylo <- phylo[phylo$protective != '#{seq.split(/_/, 2)[1]}',]"
      end
      handle.puts "phylo <- rm.dists(phylo)"
    end
  end

  resHash = tree.outR(includedSeq, handle)
  names = resHash["full"].map { |label| "\"#{label}\"" }.join(", ")
  File.open(savetreefile, "w") do |h2|
      tree.tree(includedSeq, h2, resHash["full"].reverse)
      h2.puts <<BLOCK

# Some blurb that seems to do what we want in figtree
begin figtree;
	set appearance.backgroundColorAttribute="User Selection";
	set appearance.backgroundColour=#-1;
	set appearance.branchColorAttribute="User Selection";
	set appearance.branchLineWidth=1.0;
	set appearance.foregroundColour=#-16777216;
	set appearance.selectionColour=#-2144520576;
	set branchLabels.colorAttribute="User Selection";
	set branchLabels.displayAttribute="Control";
	set branchLabels.fontName="sansserif";
	set branchLabels.fontSize=8;
	set branchLabels.fontStyle=0;
	set branchLabels.isShown=true;
	set branchLabels.significantDigits=4;
	set layout.expansion=0;
	set layout.layoutType="RECTILINEAR";
	set layout.zoom=0;
	set nodeBars.barWidth=4.0;
	set nodeLabels.colorAttribute="User Selection";
	set nodeLabels.displayAttribute="Node ages";
	set nodeLabels.fontName="sansserif";
	set nodeLabels.fontSize=8;
	set nodeLabels.fontStyle=0;
	set nodeLabels.isShown=false;
	set nodeLabels.significantDigits=4;
	set polarLayout.alignTipLabels=true;
	set polarLayout.angularRange=0;
	set polarLayout.rootAngle=0;
	set polarLayout.rootLength=100;
	set polarLayout.showRoot=true;
	set radialLayout.spread=0.0;
	set rectilinearLayout.alignTipLabels=true;
	set rectilinearLayout.curvature=0;
	set rectilinearLayout.rootLength=100;
	set scale.offsetAge=0.0;
	set scale.rootAge=1.0;
	set scale.scaleFactor=1.0;
	set scale.scaleRoot=false;
	set scaleAxis.automaticScale=true;
	set scaleAxis.fontSize=8.0;
	set scaleAxis.isShown=false;
	set scaleAxis.lineWidth=1.0;
	set scaleAxis.majorTicks=1.0;
	set scaleAxis.origin=0.0;
	set scaleAxis.reverseAxis=false;
	set scaleAxis.showGrid=true;
	set scaleAxis.significantDigits=4;
	set scaleBar.automaticScale=true;
	set scaleBar.fontSize=10.0;
	set scaleBar.isShown=false;
	set scaleBar.lineWidth=1.0;
	set scaleBar.scaleRange=20.0;
	set scaleBar.significantDigits=4;
	set tipLabels.colorAttribute="User Selection";
	set tipLabels.displayAttribute="Names";
	set tipLabels.fontName="sansserif";
	set tipLabels.fontSize=8;
	set tipLabels.fontStyle=0;
	set tipLabels.isShown=true;
	set tipLabels.significantDigits=4;
	set trees.order=false;
	set trees.orderType="increasing";
	set trees.rooting=false;
	set trees.rootingType="User Selection";
	set trees.transform=false;
	set trees.transformType="cladogram";
end;

BLOCK
  end
  
handle.puts <<BLOCK
  if (any(phylo[[rev(all.terms)[1]]]))
    {
      print(data.frame(challenge=
            phylo[phylo[[rev(all.terms)[1]]],]$challenge,
            protective=phylo[phylo[[rev(all.terms)[1]]],]$protective))
      stop("Error - out of tree controls!")
    }
    phylo
  }

phylo.get.#{saveroot.gsub(/-/, "_")}.terms <- function()
  {
    all.tree.terms <- c(#{names})

    paste("control", all.tree.terms, sep=".")
  }
BLOCK

end

