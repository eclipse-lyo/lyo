/*******************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 *
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 *  The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 *  and the Eclipse Distribution License is available at
 *  http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *  Contributors:
 *
 *     Subhajit Bhuiya <subhuiya@in.ibm.com>     - initial implementation
 *******************************************************************************/
package org.eclipse.lyo.client.oslc.samples.automation;

import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

public class MachineInformation {
	
	private String hostname = null;
	private String ipAddress = null;
	private String macAddress = null;
	private String FQDN = null;
	private Map<InetAddress, NetworkInterface> addrsIPv4 = null;
	private Map<InetAddress, NetworkInterface> addrsIPv6 = null;
	
	public MachineInformation() {
		// Set all four attributes
		try {
			NetworkInterface iface = null;
			FQDN = InetAddress.getLocalHost().getCanonicalHostName();
			hostname = InetAddress.getLocalHost().getHostName();
			
			/*
			 * Get the default IP address and network interface for the machine.
			 * If the default IP address is the loopback address then we'll
			 * query all the network interfaces configured in the system and
			 * return one of the routable IP addrs found and it's corresponding
			 * network interface.
			 */
			if (!InetAddress.getLocalHost().isLoopbackAddress()) {
				ipAddress = InetAddress.getLocalHost().getHostAddress();
				iface = NetworkInterface.getByInetAddress(InetAddress.getLocalHost());
			}
			else {
				/*
				 * Get IP addrs for all network interfaces, we'll set ipAddress to
				 * the first IPv4 address we find. If no IPv4 addrs are configured
				 * we'll use the first IPv6 addrs found, if any.
				 */
				if (getInterfaceIpAddrs(null)) {
					Map<InetAddress, NetworkInterface> addrMap = null;
					if (!addrsIPv4.isEmpty())
						addrMap = addrsIPv4;
					else
						addrMap = addrsIPv6;
					for (InetAddress addr : addrMap.keySet()) {
						ipAddress = addr.getHostAddress();
						iface = addrMap.get(addr);
						break;
					}
				}
				else {
					// If we didn't find any routable IP addresses, we'll just have to use the default/loopback address
					ipAddress = InetAddress.getLocalHost().getHostAddress();
					iface = NetworkInterface.getByInetAddress(InetAddress.getLocalHost());
				}
			}
			
			/*
			 * We want to set the mac address here, but can't until we are using
			 * 1.6 JRE. Until then we'll just use the interface name, ie. eth0,
			 * le0, etc
			 */
			if (iface != null) {
				// macAddress = Arrays.toString(iface.getHardwareAddress());
				macAddress = iface.getName();
			} else {
				macAddress = "Unknown";
			}
		} catch (UnknownHostException e) {
			hostname = "AdapterHost";
		} catch (SocketException e) {
			macAddress = "Unknown";
		}
	}

	public String getHostname() {
		if (hostname == null){
			hostname = "localhost";
		}
		return hostname;
	}

	public String getIpAddress() {
		if (ipAddress == null){
			ipAddress = "127.0.0.1";
		}
		return ipAddress;
	}

	public String getMacAddress() {
		if (macAddress == null){
			macAddress = "";
		}
		return macAddress;
	}

	public String getFQDN() {
		if (FQDN == null){
			FQDN = "localhost";
		}
		return FQDN;
	}

	/**
	 * Get IP addresses for one interface or for all interfaces if name is null.
	 * We exclude loopback addresses. Any IP addrs found are stored in HashMaps
	 * named addrsIPv4 and addrIPv6.
	 * 
	 * @param name
	 *            of the NetworkInterface to search for IP addresses (ie. eth0,
	 *            le0, etc) or null to search all configured interfaces.
	 * @return true if any non-loopback IP addresses are configured
	 * @throws SocketException 
	 */
	private boolean getInterfaceIpAddrs(String name) throws SocketException {
		boolean IPAddrsFound = false;
		if (addrsIPv4 == null)
			addrsIPv4 = new HashMap<InetAddress, NetworkInterface>();
		else 
			addrsIPv4.clear();
		if (addrsIPv6 == null)
			addrsIPv6 = new HashMap<InetAddress, NetworkInterface>();
		else 
			addrsIPv6.clear();
		
		Enumeration<NetworkInterface> e = NetworkInterface.getNetworkInterfaces();
		while(e.hasMoreElements()) {
			NetworkInterface ni = (NetworkInterface)e.nextElement();
			if (name == null || name.length() == 0 || ni.getName().equalsIgnoreCase(name)) {
				Enumeration<InetAddress> ips = ni.getInetAddresses();
				while(ips.hasMoreElements()) {
					InetAddress ip = ips.nextElement();
					// If the address is a Loopback, link-local, or site-local address, skip it
					if (ip.isLoopbackAddress() || ip.isLinkLocalAddress() || ip.isSiteLocalAddress())
						continue;
					// If IPv6 address, check to see if we want it
					if (ip instanceof Inet6Address)
						addrsIPv6.put(ip, ni);							
					else 
						addrsIPv4.put(ip, ni);							
					IPAddrsFound = true;
				}
			}
		}
		return IPAddrsFound;
	}

}

