/*
 * Copyright (c) 2016-2017   KTH Royal Institute of Technology.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * Contributors:
 *
 * Omar Kacimi         -  Initial implementation
 * Andrew Berezovskyi  -  Lyo contribution updates
 */
package org.eclipse.lyo.oslc4j.trs.consumer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.PropertyConfigurator;
import org.eclipse.lyo.oslc4j.trs.consumer.TRSProvider.handler.ConcurrentTRSProviderHandler;
import org.eclipse.lyo.oslc4j.trs.consumer.TRSProvider.handler.TrsProviderHandler;
import org.eclipse.lyo.oslc4j.trs.consumer.concurrent.TRSScheduledExecutorService;
import org.eclipse.lyo.oslc4j.trs.consumer.httpclient.TRSHttpClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The main entry point to the consumer. This class sets up the configuration ,loads the TRS
 * Providers from their respective configuration files and starts the periodic loop including all
 * the involved TRS Providers
 *
 * @author Omar
 */

public class TRSClient {
    final static Logger logger = LoggerFactory.getLogger(TRSClient.class);
    /*
     * Sparql credentials to comunicate with the triplestore and sparql
     * endpoints of the triplestore
     */
    public static String sparqlUpdateBase = "";
    public static String sparqlQueryBase = "";
    public static String sparql_user = "";
    public static String sparql_pwd = "";

    public static String PROP_SPARQLQUERYBASE = "sparqlQueryBase";
    public static String PROP_SPARQLUPDATEBASE = "sparqlUpdateBase";

    public static String PROP_LOGLEVEL = "LogLevel";

    public static String PROP_BASEAUTHUSER = "baseAuth_user";
    public static String PROP_BASEAUTHPWD = "baseAuth_pwd";

    public static String trsClientFolderName = "TrsClient";
    public static String trsClientUnixFolderName = ".TrsClient";
    public static String trsClientlogFileName = "TRSClient.log";
    /*
     * List of the TRS providers. A TRS provider is created for each TRS
     * provider properties file found in the TRS PRoviders configuration folder
     */
    public static List<TrsProviderHandler> trsProviders = new ArrayList<TrsProviderHandler>();
    private static String trsClientConfigFileName = "trsClient.properties";
    private static String fileSeparator = File.separator;
    /*
     * Log level read from the configuration file of the TRS Consumer from the
     * user
     */
    private static String logLevel;

    /*
     * http client instance shared between all the TRS providers
     */
    private static TRSHttpClient trsHttpClient = new TRSHttpClient();

    /*
     * Config files locations. These are expected to be in a specific user
     * defined location which is different according to the OS on which the TRS
     * provider is deployed. Please check the documentation for more information
     */
    private static String appDataPath;
    private static String trsProvidersConfigPath;

    private static String trsClientConfigPath;
    private static String configPath;
    private static String logFilePath;

    /**
     * Create the TRS providers instances by loading the configuration information of the individual
     * TRS providers from their config files
     */
    public static void loadTrsProviders() {

        File trsClientConfigsParentFile = new File(trsProvidersConfigPath);

        InputStream input = null;

        for (File trsProviderConfigFile : trsClientConfigsParentFile.listFiles()) {
            try {

                input = new FileInputStream(trsProviderConfigFile);

                Properties prop = new Properties();
                prop.load(input);

                String baseAuth_user = prop.getProperty("baseAuth_user");
                String baseAuth_pwd = prop.getProperty("baseAuth_pwd");

                String trs_uri = prop.getProperty("trs_uri");
                if (trs_uri != null) {
                    TrsProviderHandler trsProvider = new ConcurrentTRSProviderHandler(trs_uri,
                            sparqlQueryBase, sparqlUpdateBase, trsHttpClient, baseAuth_user,
                            baseAuth_pwd, sparql_user, sparql_pwd);
                    trsProviders.add(trsProvider);
                } else {
                    logger.error(
                            "trs provider config file at :" + trsProviderConfigFile.getName() +
                                    "" + " seems to be corrupt. No trs_uri has been provided");
                }

            } catch (IOException ex) {
                logger.error("Error reading configuration file '{}'",
                        trsProviderConfigFile.getName(), ex);
            } finally {
                if (input != null) {
                    try {
                        input.close();
                    } catch (IOException e) {
                        logger.error("Error closing file input for file '{}'",
                                trsProviderConfigFile.getName(), e);
                    }
                }
            }
        }

    }

    /**
     * loads the client properties including the sparql endpoints and the credentials to use for the
     * sparql http endpoints
     *
     * @return returns false in case the configuration info is loaded successfuly and false
     * otherwise
     */
    public static boolean loadTrsClientProps() {
        Properties prop = new Properties();
        InputStream input = null;

        try {
            File trsClientConfigFile = new File(trsClientConfigPath);
//            trsClientConfigFile.getAbsolutePath();
            input = new FileInputStream(trsClientConfigFile);

            prop.load(input);

            sparqlUpdateBase = prop.getProperty(PROP_SPARQLUPDATEBASE);
            sparqlQueryBase = prop.getProperty(PROP_SPARQLQUERYBASE);

            logLevel = prop.getProperty(PROP_LOGLEVEL);

            sparql_user = prop.getProperty(PROP_BASEAUTHUSER);
            sparql_pwd = prop.getProperty(PROP_BASEAUTHPWD);

        } catch (IOException ex) {
            logger.error("error loading properties from '{}'", trsClientConfigPath, ex);
        } finally {
            if (input != null) {
                try {
                    input.close();
                } catch (IOException e) {
                    logger.error("error closing the stream", e);
                }
            }
        }
        if (sparqlQueryBase == null || sparqlQueryBase.isEmpty() || sparqlUpdateBase.isEmpty() ||
                sparqlUpdateBase == null) {
            return false;
        }
        return true;

    }

    /**
     * 1. cconfiguration of the logging level given by the user from the trs client config file 2.
     * loading of the TRS providers from the config folder and creation of the TRS providers objects
     * 3. Starting the TRS providers loop
     */
    public static void main(String[] args) {

        defaultDirectory();

        configPath = appDataPath + fileSeparator + "config";
        logFilePath = appDataPath + fileSeparator + "log" + fileSeparator + trsClientlogFileName;

        trsClientConfigPath = configPath + fileSeparator + trsClientConfigFileName;
        trsProvidersConfigPath = configPath + fileSeparator + "trsProviders";

        boolean trsClientConfig = loadTrsClientProps();

        updateLog4jConfiguration(logFilePath);
        if (!trsClientConfig) {
            logger.error(
                    "there was problem while loading the configuration of the TRS client. Please " +
                            "" + "" + "" + "" + "" + "" + "" + "check that you provided a valid "
                            + "configuration");
        }

        loadTrsProviders();

        if (!trsProviders.isEmpty()) {
            final ScheduledExecutorService scheduler = new TRSScheduledExecutorService(
                    trsProviders.size());

            for (TrsProviderHandler trsProvider : trsProviders) {
                scheduler.scheduleAtFixedRate(trsProvider, 5, 60, TimeUnit.SECONDS);
            }
        }

    }

    /**
     * Set the log level to the value provide by the user in the config file and the log file
     * appender to the appropriate location
     *
     * @param logFile path to which the file appender file param is set
     */
    private static void updateLog4jConfiguration(String logFile) {

        Properties props = new Properties();
        try {
            InputStream configStream = TRSClient.class.getResourceAsStream("/log4j.properties");
            props.load(configStream);
            configStream.close();
        } catch (IOException e) {
            logger.error("Errornot laod configuration file ");
        }
        props.put("log4j.appender.FILE.file", logFile);
        LogManager.resetConfiguration();
        PropertyConfigurator.configure(props);
        if (logLevel != null && !logLevel.isEmpty()) {
            LogManager.getRootLogger().setLevel(getLoggingLevel(logLevel));
        }
    }

    /**
     * @param loggingLevelString log level string value given by the user and read from the config
     *                           file
     *
     * @return a log4j logging level according to the log level input from the user
     */
    private static Level getLoggingLevel(String loggingLevelString) {
        switch (loggingLevelString) {
            case "TRACE":
                return Level.TRACE;
            case "DEBUG":
                return Level.DEBUG;

            case "INFO":
                return Level.INFO;

            case "WARN":
                return Level.WARN;

            case "ERROR":
                return Level.ERROR;

            case "FATAL":
                return Level.FATAL;

            case "OFF":
                return Level.OFF;

            case "ALL":
                return Level.ALL;
            default:
                break;
        }
        return null;
    }

    /**
     * According to the os return the directory used to store the application data including the trs
     * client configuration and the logging
     */
    private static void defaultDirectory() {
        String OS = System.getProperty("os.name").toUpperCase();

        if (OS.contains("WIN")) {
            appDataPath = System.getenv("APPDATA");
            appDataPath = appDataPath + fileSeparator + trsClientFolderName;
        } else if (OS.contains("MAC")) {
            appDataPath = System.getProperty("user.home") + "/Library/Application " + "Support";
            appDataPath = appDataPath + fileSeparator + trsClientFolderName;
        } else if (OS.contains("NUX")) {
            appDataPath = System.getProperty("user.home");
            appDataPath = appDataPath + fileSeparator + trsClientUnixFolderName;
        }

    }

}
