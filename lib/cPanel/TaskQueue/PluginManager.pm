package cPanel::TaskQueue::PluginManager;

use strict;
use cPanel::TaskQueue ();

my %plugins_list;

sub load_all_plugins {
    my %opts = @_;

    die "No directory list supplied.\n" unless exists $opts{'directories'} and 'ARRAY' eq ref $opts{'directories'};
    die "No namespace list supplied.\n" unless exists $opts{'namespaces'}  and 'ARRAY' eq ref $opts{'namespaces'};
    foreach my $dir ( @{ $opts{'directories'} } ) {
        foreach my $ns ( @{ $opts{'namespaces'} } ) {
            load_plugins( $dir, $ns );
        }
    }
}

sub load_plugins {
    my ( $root_dir, $namespace ) = @_;

    die "No directory supplied for finding plugins.\n"                      unless defined $root_dir and length $root_dir;
    die "Supplied directory '$root_dir' does not exist.\n"                  unless -d $root_dir;
    die "Supplied directory '$root_dir' not part of Perl's include path.\n" unless grep { $_ eq $root_dir } @INC;

    die "No namespace for plugins specified.\n" unless defined $namespace and length $namespace;
    die "Namespace '$namespace' not a valid Perl namespace.\n"
      unless $namespace =~ m{^ \w+ (?: :: \w+ )* $}x;

    my $ns_dir = join( '/', $root_dir, split( '::', $namespace ) );

    # not having the namespace in that root is not an error.
    return unless -d $ns_dir;

    opendir( my $dir, $ns_dir ) or die "Unable to read directory '$ns_dir': $!\n";
    my @files = grep { !/^\.\.?$/ } readdir($dir);
    closedir($dir) or die "Failed to close directory '$ns_dir': $!\n";

    # TODO: Do we want to handle subdirectories?
    my @modules = map { ( /^(\w+)\.pm$/ and -f "$ns_dir/$_" ) ? $1 : () } @files;
    foreach my $mod (@modules) {
        load_plugin_by_name( $namespace . '::' . $mod );
    }
}

sub load_plugin_by_name {
    my ($modname) = @_;

    # Don't try to reload.
    return 1 if exists $plugins_list{$modname};

    eval "require $modname;";    ## no critic (ProhibitStringyEval)
    if ($@) {
        warn "Failed to load '$modname' plugin: $@\n";
        return;
    }

    my $register = UNIVERSAL::can( $modname, 'to_register' );
    unless ( defined $register ) {
        warn "Plugin '$modname' not registered, no 'to_register' method.\n";
        return;
    }
    my $num_reg = 0;
    my @commands;
    foreach my $reg ( $register->() ) {
        unless ( 'ARRAY' eq ref $reg and 2 == @{$reg} ) {
            warn "Plugin '$modname': invalid registration entry\n";
            next;
        }
        eval { cPanel::TaskQueue->register_task_processor( @{$reg} ); } or do {
            warn "Plugin '$modname' register failed: $@\n";
            next;
        };
        ++$num_reg;
        push @commands, $reg->[0];    # Add command name to list.
    }

    if ($num_reg) {
        $plugins_list{$modname} = \@commands;
        return 1;
    }
    return;
}

sub list_loaded_plugins {
    return keys %plugins_list;
}

sub get_plugins_hash {
    my %clone;
    while ( my ( $module, $commands ) = each %plugins_list ) {
        $clone{$module} = [ @{$commands} ];
    }
    return \%clone;
}

1;

__END__


=head1 NAME

cPanel::TaskQueue::PluginManager - Supplies support for loading the Queue task processing plugins.

=head1 SYNOPSIS

   use cPanel::TaskQueue::PluginManager;

   # Loads the modules found in '/usr/local/cpanel/cPanel/TaskQueue/Plugin' assuming their
   #  names are cPanel::TaskQueue::Plugin::*.
   cPanel::TaskQueuePlugin::Manager::load_plugins( '/usr/local/cpanel', 'cPanel::TaskQueue::Plugin' );

   # If you have multiple directories and/or namespaces, you can use the convenience method
   #   Smart enough not to try to reload any that already match.
   cPanel::TaskQueue::PluginManager::load_all_plugins(
       directories => [ '/usr/local/cpanel', '.', '/home/fred/tasks' ],
       namespaces => [ 'cPanel::TaskQueue::Plugin', 'MyHandlers' ]
    );

    my @loaded = cPanel::TaskQueue::PluginManager::list_loaded_plugins();

=head1 DESCRIPTION

This module wraps up the logic to load any modules located in a particular
directory as plugins for the L<cPanel::TaskQueue> class. It also registers them
with that class.

=head1 INTERFACE

The interface for this module consists of three functions.

=over 4

=item cPanel::TaskQueue::PluginManager::load_plugins( $dir, $namespace )

This function loads all modules described by the supplied parameters and attempts to
register them with C<cPanel::TaskQueue>. The two required parameters are

=over 4

=item I<root_dir>

The I<root_dir> required parameter defines a directory used to find plugins. I<root_dir>
must be in C<@INC> in order for perl to load the plugin modules.

=item I<namespace>

This parameter specifies the namespace in which to find the plugin module. The
namespace is needed to properly resolve the name of the module.

=back

The actual directory where the plugin modules are located is made by combining
the I<root_dir> and I<namespace> the same way Perl does. The '::' characters in
the namespace are replaced with path separators. This relative path is combined
with the I<root_dir> to create the actual path we search.

=item cPanel::TaskQueue::PluginManager::load_all_plugins( directories => $dirs, namespaces => $ns )

This function helps to deal with distributed sets of plugins. It requires two
named arguments:

=over 4

=item I<directories>

A reference to an array of root directories. These directories are used as the
I<root_dir> parameter to the C<load_plugins> method.

=item I<namespaces>

A reference to an array of namespaces. These namespaces are used as the I<namespace>
parameter to the C<load_plugins> method.

=back

This method just calls the C<load_plugins> function with every combination of
directory and namespace supplied. This allows multiple directories (maybe system,
site, and special) to be searched for plugins. The plugins may also be separated
into multiple namespaces (maybe C<cPanel::TaskQueue::Plugin>, C<Site::Tasks>,
C<cPanel::Backup::Tasks>).

Using the examples above, would generate 9 different locations to search for
plugins and load any found in those locations.

=item cPanel::TaskQueue::PluginManager::load_plugin_by_name( $module_name )

Loads only the plugin defined by the specified full Perl module name. The
directory containing the module must already be part of the Perl path.

Returns a true value is successful and a false value otherwise.

=item cPanel::TaskQueue::PluginManager::list_loaded_plugins()

Returns a list of the names of the loaded plugins in no particular order.

=item cPanel::TaskQueue::PluginManager::get_plugins_hash()

Returns a reference to a hash listing the loaded plugins. The hash maps the
module names of loaded plugins to an anonymous array of the commands that the
module provides.

=back

=head1 DIAGNOSTICS

=over

=item C<< No directory list supplied. >>

C<load_all_plugins> was called without a I<directories> parameter.

=item C<< No namespace list supplied. >>

C<load_all_plugins> was called without a I<namespaces> parameter.

=item C<< No directory supplied for finding plugins. >>

C<load_plugins> was called without the required I<rootdir> parameter.

=item C<< Supplied directory '%s' does not exist. >>

The root directory parameter passed to C<load_plugins> was not a valid directory.

=item C<< Supplied directory '%s' not part of Perl's include path. >>

The root directory parameter passed to C<load_plugins> is not part of Perl's
include path. Plugins in that directory will not be able to be loaded.

=item C<< No namespace for plugins specified. >>

C<load_plugins> was called without the required I<namespace> parameter.

=item C<< Namespace '%s' not a valid Perl namespace. >>

The string passed to C<load_plugins> as a namespace is not a valid namespace.

=item C<< Failed to load '%s' plugin: %s >>

Attempting to load the named plugin module failed for the reason given.

=item C<< Plugin '%s' not registered, no 'to_register' method. >>

The named plugin does not supply the C<to_register> package method. Without
this method, we cannot register the plugin with the C<cPanel::TaskQueue>
module.

This may mean that the named module is intended to be a C<TaskQueue> plugin.

=item C<< Plugin '%s': invalid registration entry >>

The C<to_register> package method of the named plugin returned data that was
inconsistent with registering a C<TaskQueue> plugin. This method is expected to
return a list of arrayrefs. Each of these arrayrefs should be a two item list.

Either the named module is not a C<TaskQueue> plugin or the C<to_register> method
needs to be fixed.

=item C<< Plugin '%s' register failed: %s >>

Registering a command returned by C<to_register> failed for the reason given.

=back

=head1 CONFIGURATION AND ENVIRONMENT

cPanel::TaskQueue::PluginManager requires no configuration files or environment
variables.

=head1 DEPENDENCIES

cPanel::TaskQueue

=head1 SEE ALSO

cPanel::TaskQueue::TaskProcessor

=head1 INCOMPATIBILITIES

None reported.

=head1 BUGS AND LIMITATIONS

At present, this module does not have any support for enabling/disabling individual
plugins.  That will probably be an important feature at some point.

=head1 AUTHOR

G. Wade Johnson  C<< wade@cpanel.net >>

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2014, cPanel, Inc. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
