<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<!--
 * Copyright (c) 2012 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *  
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *     Keith Wells             - initial API and implementation
 *     Sam Padgett           - initial API and Implementation
 *     Jim Conallen           - initial API and implementation
 *
 -->
<html><head><link rel="SHORTCUT ICON" href="oslc.png"><title>Login Page</title></head>
<body>
OSLC Login
<hr/>

<form action="j_security_check" method="post">
<table>
 <tr><td>Name:</td><td><input type="text" name="j_username"></td></tr>
 <tr><td>Password:</td><td><input type="password" name="j_password" ></td></tr>
</table>
<br/>
<input type="submit" value="login"> 
</form>
</body>
</html>