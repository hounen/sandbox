cd %~dp0
knime.exe -nosave -nosplash -reset -consoleLog -application org.knime.product.KNIME_BATCH_APPLICATION -workflowFile="KNIME_project.knwf"
exit