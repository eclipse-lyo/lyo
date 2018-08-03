package org.eclipse.lyo.oslc4j.provider.jena;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Helper class for scanning the classes inside a package.
 * @author rherrera
 */
public class ClassScanner {
    /**
     * Extracts the classes from a given package name.
     * @param pkg the fully-qualified name of the package to scan.
     * @return the set of classes within the package if any.
     * @throws FileNotFoundException if package is not on classpath.
     * @throws IOException if some I/O exception occurs.
     * @throws ClassNotFoundException if a named class does not exists.
     */
    public static Set<Class<?>> getClassesOnPackage(String pkg)
            throws IOException, ClassNotFoundException {
        Enumeration<JarEntry> entries;
        Set<Class<?>> classes = new HashSet<>();
        String jar, name, pckg = pkg.replaceAll("\\.", "/");
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        URL path = loader.getResource(pckg);
        if (path == null) throw new FileNotFoundException(pckg);
        jar = path.getFile();
        if (jar.contains("!")) {
            jar = jar.substring(jar.indexOf(":") + 1, jar.indexOf("!"));
            entries = new JarFile(jar).entries();
            while(entries.hasMoreElements()) {
                name = entries.nextElement().getName();
                if (name.startsWith(pckg)) {
                    if (name.endsWith(".class") && !name.contains("$")) {
                        name = name.substring(0, name.indexOf(".class"));
                        classes.add(Class.forName(name.replaceAll("/", ".")));
                    }
                }
            }
        } else {
            File directory = new File(jar);
            String[] content = directory.list();
            for (String file : content) {
                if (file.endsWith(".class") && !file.contains("$")) {
                    file = file.substring(0, file.indexOf(".class"));
                    classes.add(Class.forName(pkg + "." + file));
                }
            }
        }
        return classes;
    }
}